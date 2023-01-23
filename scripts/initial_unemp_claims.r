library(tidyverse)
library(lubridate)
theme_set(theme_light())

source("functions.r")

initialclaims <- get_index("ICSA") %>%
    arrange(date) %>%
    mutate(month = month(date), year = year(date)) %>%
    rename(claims = value) %>%
    select(-index)

cutoff_date <- ymd(20140101)
base_years <- c(2017, 2018, 2019)


sigma <- 4
q0 <- pnorm(sigma / 2)

annot <-
    glue::glue("{sigma}\U03C3 based on period\nFY{min(base_years)}-{max(base_years)}") # nolint

quants_2019 <-
    with(
        initialclaims,
        quantile(
            claims[year %in% base_years],
            c(q0, 1 - q0)
        )
    )

initialclaims %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(date, claims) +
    geom_line(color = "gray30", alpha = .8, size = .6) +
    geom_hline(
        yintercept = quants_2019,
        size = 2, alpha = .3, color = "gray50"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
        breaks = 100e3 * 1:100,
        labels = scales::label_number(
            scale = 1e-3,
            suffix = " K"
        )
    ) +
    coord_cartesian(ylim = c(0, 6e5)) +
    labs(
        x = "", y = "",
        title = "Weekly initial unemployment claims",
        caption = "Source: FRED St. Louis ICSA data\nLine shows 26-week rolling average") + # nolint
    annotate("text",
        x = cutoff_date + months(1),
        y = mean(quants_2019), label = annot,
        hjust = 0, color = "gray50", alpha = .8,
        size = 3
    ) +
    geom_segment(aes(
        x = cutoff_date, xend = cutoff_date,
        y = 1.05 * min(quants_2019), yend = 0.95 * max(quants_2019)
    ),
    color = "gray85", alpha = .3, size = 2,
    ) +
    geom_line(
        aes(y = zoo::rollmean(claims, 26, na.pad = TRUE, align = "center")),
        color = "gray50", alpha = .2, size = 3
    ) +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    )

ggsave("graphs/initial_unemp_claims.png", width = 8, height = 5)