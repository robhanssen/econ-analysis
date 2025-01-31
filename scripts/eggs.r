library(tidyverse)
library(lubridate)
theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    ))

source("functions.r")

# CUSR0000SAF11: Consumer Price Index for All Urban Consumers: Food at Home in U.S. City Average    # nolint
# CUSR0000SEFV: Consumer Price Index for All Urban Consumers: Food Away from Home in U.S. City Average # nolint
# CUSR0000SAF113: Consumer Price Index for All Urban Consumers: Fruits and Vegetables in U.S. City Average # nolint


inflation <- retrieve_data(index = c("APU0000708111", "CPIAUCSL")) %>%
    pivot_wider(names_from = "index", values_from = value) %>%
    filter(date >= ymd("20140101")) %>%
    mutate(
        eggs_cpi_adj = APU0000708111 * first(CPIAUCSL) / CPIAUCSL
    )

plot_eggs <- 
    inflation %>%
    filter(date > ymd(20120101)) %>%
    ggplot() +
    geom_line(aes(x = date, y = APU0000708111), color = "gray50") +
    scale_y_continuous(
        labels = scales::label_dollar(accuracy = .01),
        limits = c(0, NA)
    ) +
    labs(
        x = "", y = "Price of eggs (in USD)",
        title = "Price of eggs",
        caption = "Source: FRED APU0000708111" # nolint
    )

ggsave("graphs/egg_inflation.png",
    width = 6, height = 4,
    plot = plot_eggs
)
