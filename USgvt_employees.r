library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

gvt <- retrieve_data("USGOVT", "FRED")

gvt_plot <-
    gvt %>%
    mutate(floordate = floor_date(date, unit = "quarter")) %>%
    left_join(presidentinfo, by = "floordate") %>%
    fill(party, .direction = "down")


gvt_plot %>%
    filter(date > ymd("1959-12-31")) %>%
    ggplot() +
    aes(date, value, color = party, group = TRUE) +
    geom_line() +
    scale_y_continuous(
        breaks = seq(0, 100, 4) * 1000,
        limits = c(6000, NA),
        labels = scales::number_format(accurary = 1, scale = 1/1000)
    ) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(
        x = "Date",
        y = "US government employees (in millions)",
        caption = "Dashed vertical line indicate inauguration dates",
        title = "US government employees since 1960"
    ) +
    scale_color_manual(values = partycolor) +
    theme(legend.position = "none")

ggsave("graphs/us-gvt-employ-since-1960.png",
    width = 8,
    height = 6
)

