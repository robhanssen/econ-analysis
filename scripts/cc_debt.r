library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            panel.grid.minor.y = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

source("functions.r")

raw_data <-
    retrieve_data(
        c(
            "CCLACBW027SBOG",
            "CPIAUCSL",
            "POP",
            "GDP"
        )
    )

cpi_year <- "2012"

ccdebt_processed <-
    raw_data %>%
    pivot_wider(names_from = index, values_from = value) %>%
    rename(
        ccdebt = CCLACBW027SBOG,
        cpi = CPIAUCSL,
        pop = POP,
        gdp = GDP
    ) %>%
    arrange(date) %>%
    mutate(across(
        !date,
        ~ zoo::na.approx(.x, na.rm = FALSE)
    )) %>%
    fill(c("cpi", "pop", "gdp"), .direction = "downup") %>%
    filter(date >= ymd(glue::glue("{cpi_year}-01-01"))) %>%
    mutate(
        ccdebt_cpi = ccdebt * first(cpi) / cpi,
        ccdebt_cpi_pop = ccdebt_cpi * first(pop) / pop,
        ccdebt_pop = ccdebt * first(pop) / pop,
    ) %>%
    drop_na() # remove NAs caused by POP predictions


long_format_data <-
    ccdebt_processed %>%
    pivot_longer(starts_with("ccdebt"),
        names_to = "index",
        values_to = "value"
    )

labels <-
    long_format_data %>%
    slice_max(date, by = index) %>%
    select(date, value, index)

cc_debt_g <-
    long_format_data %>%
    ggplot(aes(x = date, y = value, color = index)) +
    geom_line() +
    scale_x_date(
        date_labels = "%Y",
        date_breaks = "4 years",
        date_minor_breaks = "1 year"
    ) +
    scale_y_continuous(
        limits = c(500, NA),
        labels = scales::dollar_format(scale = 1, suffix = " B")
    ) +
    ggrepel::geom_label_repel(
        data = labels,
        hjust = 1,
        aes(x = date, y = value, label = index)
    ) +
    labs(
        title = glue::glue("(Adjusted) credit card debt growth between {cpi_year} and {year(today())}"),
        x = "", y = "Adjusted credit-card debt (in $)",
        caption = glue::glue(
            "Source: FRED CCLACBW027SBOG and CPIAUCSL\n",
            "CPI-adjusted amounts in {cpi_year} dollars"
        )
    ) +
    theme(legend.position = "none")

av2019 <- long_format_data %>%
    filter(index == "ccdebt_cpi_pop", year(date) == 2019) %>%
    summarize(av = mean(value)) %>%
    pull(av)

av2019_line <-
    tibble(
        x = ymd(20130101, 20240101),
        y = av2019
    )

pred2022onw <- long_format_data %>%
    filter(index == "ccdebt_cpi_pop", year(date) >= 2022) %>%
    lm(value ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(
                ymd(20220101), today() + years(1), "1 month"
            ))
    )

xdate <- as.Date(
    approx(pred2022onw$.fitted, pred2022onw$date, xout = av2019)$y,
    origin = "1970-01-01"
)

xdate_g <-
    cc_debt_g +
    geom_line(
        data = pred2022onw,
        aes(x = date, y = .fitted),
        inherit.aes = FALSE,
        alpha = .25,
        color = "gray70",
        linewidth = 2
    ) +
    geom_line(
        inherit.aes = FALSE,
        data = av2019_line,
        aes(x, y),
        alpha = .25,
        color = "gray70",
        linewidth = 2
    ) +
    geom_vline(
        xintercept = xdate,
        alpha = .25,
        color = "gray70",
        linewidth = 2
    ) +
    annotate("text",
        x = xdate, y = 520, hjust = 1,
        size = 2,
        label = format(xdate, format = "%b %d,\n%Y")
    ) +
    annotate("text",
        x = ymd(20140101), y = av2019 + 10,
        hjust = 0, color = "gray70",
        size = 2.5, label = "2019 average"
    )


ggsave("graphs/ccdebt.png",
    width = 7, height = 5,
    plot = xdate_g
)
