library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

jobs <- retrieve_data("PAYEMS", "FRED") %>%
    mutate(
        year = year(date),
        month = month(date)
    ) %>%
    arrange(date) %>%
    mutate(value = value * 1000) %>%
    mutate(growth_month = value - lag(value)) %>%
    group_by(month) %>%
    mutate(growth_annual = value - lag(value)) %>%
    ungroup() %>%
    mutate(prezpd = cut(date, breaks = inaugdates)) %>%
    mutate(prez = as.character(prezpd)) %>%
    inner_join(presidentinfo %>% mutate(prez = as.character(inaugdate)))

jobs %>%
    filter(date > ymd(19301231)) %>%
    group_by(party) %>%
    mutate(party_growth = cumsum(growth_month)) %>%
    ggplot(aes(x = date, y = party_growth, color = party)) +
    geom_line(linetype = 2) +
    geom_point(shape = 1, alpha = .3, size = 1) +
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = " M")
    ) +
    scale_color_manual(values = c("R" = "red", "D" = "blue"), labels = c("Republican", "Democratic")) +
    labs(
        x = "", y = "Cumulative jobs growth by president party (in millions)",
        caption = "Source: FRED PAYEMS data",
        title = "Cumulative Jobs Growth by President Party",
        color = "President Party"
    ) +
    theme(
        plot.caption = element_text(hjust = 0, size = 8),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "inside",
        legend.justification = c(1, 0), legend.background = element_blank(),
    )

ggsave("dev/jobs_growth_by_party.png", width = 8, height = 5)


jobs %>%
    filter(date > ymd(19301231)) %>%
    group_by(party) %>%
    mutate(party_growth = cumsum(growth_month)) %>%
    slice_max(date) %>%
    select(date, party, party_growth)
