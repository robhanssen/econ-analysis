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
    geom_point(shape = 1, alpha = .3) + 
    geom_line(linetype = 2) +
    geom_hline(yintercept = 0, linewidth = 2, alpha = .8) + 
    scale_y_continuous(
        labels = scales::label_number()
    ) +
    scale_color_manual(values = c("R" = "red", "D" = "blue")) + 
    labs(x = "", y = "Cumulative jobs growth by president party", 
        caption = "Source: FRED PAYEMS data")

ggsave("dev/jobs_growth_by_party.png", width = 7, height = 5)


    jobs %>%
        filter(date > ymd(19301231)) %>%
        group_by(party) %>%
        mutate(party_growth = cumsum(growth_month)) %>%
        slice_max(date) %>%
        select(date, party, party_growth)