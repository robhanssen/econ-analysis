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


inflation <- retrieve_data(index = c(
    "CUSR0000SAF11",
    "CUSR0000SEFV",
    "CUSR0000SAF113"
)) %>%
    pivot_wider(names_from = index, values_from = value) %>%
    drop_na() %>%
    rename(
        food_at_home = CUSR0000SAF11,
        food_out = CUSR0000SEFV,
        food_fruitveg = CUSR0000SAF113
    ) %>%
    mutate(month = month(date), year = year(date)) %>%
    group_by(month) %>%
    mutate(across(2:4, ~ .x / lag(.x) - 1,
        .names = "{col}_annualinflation"
    )) %>%
    ungroup()

plot_foodinflation <- inflation %>%
    filter(date > ymd(20180101)) %>%
    ggplot() +
    geom_line(aes(x = date, y = food_at_home_annualinflation),
        color = "blue"
    ) +
    geom_line(aes(x = date, y = food_fruitveg_annualinflation),
        color = "darkgreen"
    ) +
    geom_line(aes(x = date, y = food_out_annualinflation),
        color = "maroon"
    ) +
    scale_y_continuous(labels = scales::percent_format(), breaks = 0:100 * .02)


avg_index <-
    tibble(x = ymd(20140101, 20181231), y = 1.02, year = year(x))


base <- inflation %>%
    filter(year %in% first(avg_index$year):last(avg_index$year)) %>%
    summarize(across(2:4, mean))

plot_foodhome <-
    inflation %>%
    mutate(
        food_at_home_indexed = food_at_home / base$food_at_home,
        food_fruitveg_indexed = food_fruitveg / base$food_fruitveg,
        food_out_indexed = food_out / base$food_out
    ) %>%
    filter(date > ymd(20120101)) %>%
    ggplot() +
    geom_line(aes(x = date, y = food_at_home_indexed), color = "gray50") +
    geom_errorbar(
        data = avg_index,
        width = .005,
        color = "gray70",
        aes(
            xmin = first(x), xmax = last(x),
            y = first(y)
        )
    ) +
    annotate("text",
        x = first(avg_index$x), y = first(avg_index$y) + .01,
        hjust = 0, label = "INDEX", color = "gray70"
    ) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = 1 + -100:100 * .05
    ) +
    labs(
        x = "", y = "Food price index",
        title = "Indexed food-at-home prices",
        caption = "Source: FRED CUSR0000SAF11\nIndexed to the average full-year 2014-2018" # nolint
    )

ggsave("graphs/food_inflation.png",
    width = 6, height = 4,
    plot = plot_foodhome
)

plot_foodout <-
    inflation %>%
    mutate(
        food_at_home_indexed = food_at_home / base$food_at_home,
        food_out_indexed = food_out / base$food_out
    ) %>%
    filter(date > ymd(20120101)) %>%
    ggplot() +
    geom_line(aes(x = date, y = food_out_indexed), color = "maroon") +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = 1 + -100:100 * .05
    )
