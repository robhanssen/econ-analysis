library(tidyverse)
library(lubridate)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")

inflation <- retrieve_data(c("CPIAUCSL","CPILFESL", "PCEPILFE")) %>%
    arrange(date) %>%
    mutate(month = month(date)) %>%
    group_by(month, index) %>%
    mutate(annual_inflation = value / lag(value) - 1) %>%
    ungroup()

fedrate <- get_index("DFEDTARU") %>%
    arrange(date)

inflate_label <-
    inflation %>% group_by(index) %>%
    slice_max(date, n = 1) %>% ungroup() %>%
    mutate(
        inflation = scales::percent(annual_inflation, accuracy = .1),
        label = paste0(format(date, format = "%b %Y"), "\n", index, "\n", inflation)
    )

cutoff_date <- ymd(20100101)

inflation %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(date, annual_inflation, group = index, linetype = index) +
    ggrepel::geom_label_repel(
        data = inflate_label,
        aes(
            x = date,
            y = annual_inflation,
            label = label
        ),
        hjust = "center",
        size = 2
    ) +
    geom_line() +
    scale_y_continuous(
        labels = scales::percent_format(),
        breaks = seq(0, 1, .02),
        expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(-.01, .1)) +
    scale_x_date(
        date_breaks = "3 years",
        date_labels = "%Y",
        limits = c(as.Date(NA), today() + years(1))
    ) +
    labs(
        x = "Date",
        y = "Annual inflation (in %)",
        caption = "Source: FRED CPIAUCSL, PCE and DFEDTARU"
    ) +
    geom_point(
        data = inflate_label,
        aes(
            x = date,
            y = annual_inflation
        )
    ) +
    annotate("text",
        x = max(inflate_label$date),
        y = .008,
        label = "FED\nTARGET\nRATE",
        color = "gray50",
        alpha = .3
    ) +
    geom_line(
        data = fedrate %>% filter(date >= cutoff_date),
        aes(x = date, y = value / 100), color = "gray70",
        linetype = "solid",
        alpha = .5
    ) + 
    theme(
        legend.position = "none"
    )

ggsave("graphs/us_inflation.png", width = 8, height = 6)