library(tidyverse)
library(patchwork)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")

api <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/debt_to_penny?page[size]=10000" # nolint

gen_line <- function(date_limit, period, data) {
    model <- data %>%
        filter(date %within% (date_limit[[1]] %--% date_limit[[2]])) %>%
        lm(total_public_debt ~ date, data = .)

    slope <- broom::tidy(model) %>%
        filter(term == "date") %>%
        pull(estimate)

    std_err <-
        broom::tidy(model) %>%
        filter(term == "date") %>%
        pull(std.error)

    model %>%
        broom::augment(
            newdata = tibble(
                date = seq(date_limit[[1]] - weeks(13), today(), "1 month")
            )
        ) %>%
        mutate(
            period = period,
            daily = slope,
            annual = slope * 365 * 1e-12,
            std_err = std_err * 365 * 1e-12
        )
}

gen_points <- function(date_limit, period, data) {
    data %>%
        filter(date %in% date_limit)
}

raw_json <- jsonlite::fromJSON(api)

debt <-
    as_tibble(raw_json$data) %>%
    mutate(
        across(2:9, as.numeric),
        across(1, ymd)
    ) %>%
    select(
        date = record_date,
        total_public_debt = tot_pub_debt_out_amt,
        year = record_calendar_year,
        quarter = record_calendar_quarter,
    ) %>%
    arrange(date)

gdp <-
    retrieve_data(indexes = c("GDP"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    arrange(date) %>%
    rename(gdp = "GDP")


debt_gdp <-
    full_join(debt, gdp, by = "date") %>%
    arrange(date) %>%
    mutate(gdp = zoo::na.approx(gdp, na.rm = FALSE)) %>%
    fill(gdp, .direction = "downup") %>%
    mutate(
        gdp = gdp * 1e9,
        debt_gdp = total_public_debt / gdp
    ) %>%
    drop_na(debt_gdp)

date_limits_df <-
    tribble(
        ~period, ~date_limits,
        "GW Bush", ymd(list(20020107, 20080107)),
        "Obama1", ymd(list(20090107, 20130103)),
        "Obama-Trump", ymd(list(20140107, 20190103)),
        # "Trump", ymd(list(20200701, 20210121)),
        "Trump-Biden", list(ymd(20200701), today())
    )

lines_data <-
    map_df(
        seq_len(nrow(date_limits_df)),
        ~ gen_line(
            date_limits_df$date_limits[[.x]],
            date_limits_df$period[[.x]],
            debt_gdp
        )
    )

dots_data <-
    map_df(
        seq_len(nrow(date_limits_df)),
        ~ gen_points(date_limits_df$date_limits[[.x]], data = debt_gdp)
    )

gdp_extra_g <-
    debt_gdp %>%
    filter(date > ymd(20010101)) %>%
    ggplot(aes(x = date, y = total_public_debt)) +
    geom_line() +
    scale_y_continuous(
        labels = scales::dollar_format(
            scale = 1e-12,
            suffix = "T"
        )
    ) +
    geom_point(
        data = dots_data, color = "red",
        size = 2, alpha = .7
    ) +
    geom_line(
        data = lines_data,
        aes(y = .fitted, group = period),
        lty = 2,
        color = "red"
    ) +
    geom_vline(
        xintercept = ymd(20190103) + seq(-10, 10, 1) * years(2),
        alpha = .5,
        lty = 3
    ) +
    geom_vline(xintercept = presidentinfo$inaugdate, alpha = .5) +
    labs(
        x = "", y = "Total Public Debt"
    ) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#
# adjust for CPI
#

infl <- get_index("CPIAUCSL")

infl_date <- ymd(20090101)

infl_base_year <- year(infl_date)

infl_base <- infl$value[infl$date == infl_date]

infl_corrected <-
    full_join(debt_gdp, infl, by = "date") %>%
    arrange(date) %>%
    fill(value, .direction = "down") %>%
    mutate(total_public_debt = total_public_debt * infl_base / value)

date_limits_df_infl <-
    tribble(
        ~period, ~date_limits,
        "Obama-Trump", ymd(list(20140107, 20190103)),
        "Trump-Biden", list(ymd(20200701), today() - weeks(4))
    )



lines_data_infl <-
    map_df(
        seq_len(nrow(date_limits_df_infl)),
        ~ gen_line(
            date_limits_df_infl$date_limits[[.x]],
            date_limits_df_infl$period[[.x]],
            infl_corrected
        )
    )

dots_data_infl <-
    map_df(
        seq_len(nrow(date_limits_df_infl)),
        ~ gen_points(
            date_limits_df_infl$date_limits[[.x]],
            data = infl_corrected
        ),
    )


infl_adj_g <-
    debt_gdp %>%
    filter(date > ymd(20010101)) %>%
    ggplot(aes(x = date, y = total_public_debt)) +
    geom_line() +
    scale_y_continuous(
        labels = scales::dollar_format(
            scale = 1e-12,
            suffix = "T"
        )
    ) +
    geom_line(
        data = infl_corrected %>% filter(date >= infl_date),
        inherit.aes = FALSE,
        aes(x = date, y = total_public_debt), color = "gray50"
    ) +
    geom_point(
        data = dots_data_infl, color = "red",
        size = 2, alpha = .7
    ) +
    geom_line(
        data = lines_data_infl,
        aes(y = .fitted, group = period),
        lty = 2,
        color = "red"
    ) +
    geom_vline(
        xintercept = ymd(20190103) + seq(-10, 10, 1) * years(2),
        alpha = .5,
        lty = 3
    ) +
    geom_vline(xintercept = presidentinfo$inaugdate, alpha = .5) +
    labs(
        x = "", y = "Total Public Debt",
    ) +
    annotate(
        "label",
        x = ymd(20140701), y = 13.5e12,
        color = "gray50", hjust = 0,
        label = glue::glue("Inflation-adjusted public\ndebt, in {infl_base_year} dollars") # nolint
    )

ggsave("graphs/public_debt.png",
    width = 8, height = 10,
    plot = gdp_extra_g / infl_adj_g +
        plot_annotation(
            title = "Total Public Debt over time",
            caption = glue::glue(
                "Source: Treasury.gov debt_to_penny API and FRED CPIAUCSL API\n", # nolint
                "Gray line indicates total public debt adjusted to {infl_base_year} dollars" # nolint
            )
        )
)

#
# Debt-GDP
#


debtgdp_g <-
    debt_gdp %>%
    filter(year >= 1980) %>%
    ggplot(aes(x = date, y = debt_gdp)) +
    # geom_point(shape = 1, alpha = .01) +
    geom_line(alpha = .5, linewidth = .2) +
    scale_y_continuous(limits = c(.5, 1.5)) +
    geom_vline(
        xintercept = inaugdates, lty = 1,
        alpha = .1, linewidth = 1
    ) +
    geom_vline(
        xintercept = last(gdp$date),
        lty = 2, alpha = .2
    ) +
    labs(
        x = "", y = "Debt to GDP ratio",
        title = "Ratio of Public Debt to GDP over time"
    )

ggsave("graphs/debt_gdp_usa.png",
    height = 5, width = 7, plot = debtgdp_g
)
