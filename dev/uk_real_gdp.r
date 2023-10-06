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

api <- "NGDPRSAXDCGBQ" # nolint

gen_line_exp <- function(date_limit, period, data) {
    model <- data %>%
        filter(date %within% (date_limit[[1]] %--% date_limit[[2]])) %>%
        lm(log10(ukgdp) ~ date, data = .)

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
            .fitted = 10^.fitted,
            period = period,
            daily = slope,
            annual = slope * 365 * 1e-12,
            std_err = std_err * 365 * 1e-12
        )
}


gen_line <- function(date_limit, period, data) {
    model <- data %>%
        filter(date %within% (date_limit[[1]] %--% date_limit[[2]])) %>%
        lm(ukgdp ~ date, data = .)

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

uk_gdp <- retrieve_data(api) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    filter(date >= ymd(19930101)) %>%
    rename(ukgdp = "NGDPRSAXDCGBQ") %>%
    mutate(ukgdp = 4 * ukgdp) # quarterly data to annual conversion

date_limits_df <-
    tribble(
        ~period, ~date_limits,
        "Old Period", ymd(list(19930101, 20080101)),
        "Growth", ymd(list(20090701, 20191001)),
        "Post-COVID", list(ymd(20220101), today())
    )

lines_data <-
    map_df(
        seq_len(nrow(date_limits_df)),
        ~ gen_line(
            date_limits_df$date_limits[[.x]],
            date_limits_df$period[[.x]],
            uk_gdp
        )
    )

lines_data_exp <-
    map_df(
        seq_len(nrow(date_limits_df)),
        ~ gen_line_exp(
            date_limits_df$date_limits[[.x]],
            date_limits_df$period[[.x]],
            uk_gdp
        )
    )


dots_data <-
    map_df(
        seq_len(nrow(date_limits_df)),
        ~ gen_points(date_limits_df$date_limits[[.x]], data = uk_gdp)
    )

gdp_extra_g <-
    uk_gdp %>%
    ggplot(aes(x = date, y = ukgdp)) +
    scale_y_continuous(
        labels = scales::dollar_format(
            scale = 1e-3,
            suffix = "B"
        )
    ) +
    geom_point(
        data = dots_data, color = "#a6cee3",
        size = 2, alpha = .9
    ) +
    geom_line(
        data = lines_data,
        aes(y = .fitted, group = period),
        lty = 1,
        color = "#1F78B4",
        alpha = .9
    ) +
    geom_line(
        data = lines_data_exp,
        aes(y = .fitted, group = period),
        lty = 1,
        color = "#70dd10",
        alpha = .9
    ) +
    geom_point(size = .5, alpha = .5, color = "gray50") +
    labs(
        x = "", y = "UK Real GDP (in GBP)",
        caption = glue::glue(
            "Source: Fed St. Louis FRED NGDPRSAXDCGBQ, IMF\n",
            "Blue line: linear model; Green line: exponential model"
        )
    )

ggsave("dev/ukgdp.png",
    width = 8, height = 6,
    plot = gdp_extra_g
)
