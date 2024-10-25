library(tidyverse)
library(patchwork)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")


data <- 
    retrieve_data(c("MEHOINUSA646N","CPIUFDSL")) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    set_names(c("date", "median_income","food")) %>%
    mutate(food_per_income = food / median_income) %>%
    drop_na()

data %>%
    ggplot(aes(x = date, y = food_per_income)) +
    geom_point() + geom_line()