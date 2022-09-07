library(tidyverse)
library(lubridate)
theme_set(theme_light())

source("functions.r")

debt <- get_index("GFDEBTN")
gdp <- get_index("GDP")

debt_gdp_ratio <-
    inner_join(debt, gdp, by = "date") %>%
    mutate(
        gdp_debt_ratio = value.x / value.y / 1000,
        floordate = floor_date(date, unit = "quarter")
    ) %>%
    left_join(presidentinfo, by = "floordate") %>%
    fill(president, .direction = "down") %>%
    fill(party, .direction = "down")

debt_gdp_ratio %>%
    filter(date > ymd("1979-12-31")) %>%
    ggplot() +
    aes(date, gdp_debt_ratio, color = party, group = FALSE) +
    geom_line() +
    scale_y_continuous(
        breaks = .2 * 0:100,
        limits = c(0, NA),
        labels = scales::percent_format()
    ) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(
        x = "Date",
        y = "ratio Public Debt / GDP",
        caption = "Dashed vertical line indicate inauguration dates",
        title = "Ratio of Public Debt to GDP since 1980"
    ) +
    scale_color_manual(values = partycolor) +
    theme(legend.position = "none")

ggsave("graphs/debt-to-gdp-ratio-since-1980.png",
    width = 6,
    height = 6
)