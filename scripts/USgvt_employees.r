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

abs_gov <-
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

# ggsave("graphs/us-gvt-employ-since-1960.png",
#     width = 8,
#     height = 6
# )

#
#
#

gvt_pop <- retrieve_data(indexes = c("POPTOTUSA647NWDB", "USGOVT"), src = "FRED") %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>% 
    mutate(POPTOTUSA647NWDB = zoo::na.approx(POPTOTUSA647NWDB, na.rm = FALSE)) %>% 
    fill(POPTOTUSA647NWDB) %>% 
    mutate(usgvtbypop = USGOVT / POPTOTUSA647NWDB  * 1000) %>%
    filter(!is.na(usgvtbypop)) %>%
    left_join(presidentinfo %>%
                mutate(floordate = floor_date(inaugdate, unit = "year")) %>%
                rename(date = floordate),
                by = "date") %>%
    fill(party, .direction = "down")


pop_graph <-
    gvt_pop %>%
    filter(date > ymd("1959-12-31"), !is.na(usgvtbypop)) %>%
    ggplot() +
    aes(date, usgvtbypop, color = party, group = TRUE) +
    geom_line() +
    scale_y_continuous(
        breaks = seq(0, 100, 1) / 100,
        limits = c(0, NA),
        labels = scales::percent_format(accuracy = .1)
    ) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(
        x = "Date",
        y = "US government employees as fraction of population",
        caption = "Dashed vertical line indicate inauguration dates",
        title = "US government employees since 1960",
        subtitle = "as fraction of the population"
    ) +
    scale_color_manual(values = partycolor) +
    theme(legend.position = "none")

# ggsave("graphs/us-gvt-employ-since-1960-pct.png",
#     width = 8,
#     height = 6
# )

ggsave("graphs/us-gvt-employ-since-1960.png",
    width = 12,
    height = 6,
    plot = abs_gov + pop_graph)