library(tidyverse)
library(lubridate)
theme_set(theme_light())

source("functions.r")

inflation <- get_index("CPIAUCSL") %>%
    arrange(date) %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    mutate(annual_inflation = value / lag(value) - 1) %>%
    ungroup()

inflate_label <-
    inflation %>%
    slice_max(date, n = 1) %>%
    mutate(
        inflation = scales::percent(annual_inflation, accuracy = .1),
        label = glue::glue(format(date, format = "%b %Y"), "\n{inflation}")
    )

inflation %>%
    filter(date >= ymd(20100101)) %>%
    ggplot() +
    aes(date, annual_inflation) +
    geom_line() +
    scale_y_continuous(
        labels = scales::percent_format(),
        breaks = seq(0, 1, .02),
        expand = c(0, 0)
    ) +
    scale_x_date(
        date_breaks = "3 years",
        date_labels = "%Y",
        limits = c(as.Date(NA), today() + years(1))
    ) +
    labs(
        x = "Date",
        y = "Annual inflation (in %)",
        caption = "Source: FRED CPIAUCSL"
    ) +
    ggrepel::geom_label_repel(
        data = inflate_label,
        aes(
            x = date,
            y = annual_inflation,
            label = label
        ),
        hjust = "center"
    ) +
    geom_point(
        data = inflate_label,
        aes(
            x = date,
            y = annual_inflation
        )
    ) +
    geom_hline(
        yintercept = 0.02, lty = 1,
        color = "gray50", size = 1,
        alpha = .3
    ) +
    annotate("text",
        x = inflate_label$date,
        y = .018,
        label = "FED TARGET",
        color = "gray50",
        alpha = .3
    )

ggsave("graphs/us_inflation.png", width = 8, height = 6)