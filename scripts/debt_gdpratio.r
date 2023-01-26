library(tidyverse)
library(lubridate)
theme_set(theme_light())

source("functions.r")

debt_gdp <-
    retrieve_data(indexes = c("GDP", "GFDEBTN"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    arrange(date) %>%
    fill(GFDEBTN, .direction = "down") %>%
    drop_na() %>%
    mutate(gdp_debt_ratio = GFDEBTN / GDP / 1000)

debt_gdp_ratio <-
    full_join(presidentinfo, debt_gdp, by = c("inaugdate" = "date")) %>%
    arrange(inaugdate) %>%
    fill(c("president", "party"), .direction = "down") %>%
    select(-floordate) %>%
    rename(date = inaugdate) %>%
    drop_na()

debt_gdp_ratio %>%
    filter(date > ymd("1979-12-31")) %>%
    ggplot() +
    aes(date, gdp_debt_ratio, color = party, group = FALSE) +
    geom_line(show.legend = FALSE) +
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
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    )

ggsave("graphs/debt-to-gdp-ratio-since-1980.png",
    width = 6,
    height = 4
)