library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

# get data from FRED
oilfuel <-
    inner_join(
        get_index("DCOILWTICO", "FRED") %>%
            mutate(value = value) %>%
            rename(oilprice = "value") %>%
            filter(oilprice > 0),
        get_index("GASREGW", "FRED") %>%
            rename(fuelprice = "value"),
        by = "date"
    ) %>%
    select(-index.x, -index.y)

decade_length <- 10
max_year <- max(year(oilfuel$date))
min_year <- min(year(oilfuel$date))
period <- max_year - min_year

models <-
    oilfuel %>%
    mutate(
        year = year(date),
        halfdecade = decade_length * (year %/% decade_length),
        halfdecade_str = case_when(
            halfdecade + decade_length - 1 > max_year ~
            paste0(halfdecade, "-", max_year),
            TRUE ~ paste0(halfdecade, "-", halfdecade + decade_length - 1)
        ),
        halfdecade = factor(halfdecade_str)
    ) %>%
    group_by(halfdecade) %>%
    nest() %>%
    mutate(models = map(data, ~ lm(fuelprice ~ oilprice, data = .x)))

linearanalysis <-
    models %>%
    mutate(params = map(models, broom::tidy)) %>%
    unnest(params)

# rsq_table <-
#     models %>%
#     mutate(rsq = map(models, broom::glance)) %>%
#     unnest(rsq) %>%
#     select(halfdecade, r.squared) %>%
#     mutate(rsq = paste0(
#         "R^2 = ",
#         scales::number(r.squared, accuracy = .001),
#         ""
#     ))

rsq_table <-
    models %>%
    mutate(rsq = map_dbl(seq_along(halfdecade),
           ~modelr::rsquare(models[[.x]], data[[.x]]))) %>%
    mutate(rsq = paste0(
        "R^2 = ",
        scales::number(rsq, accuracy = .001),
        ""
    ))

preddate <- tail(oilfuel %>% arrange(date), 1)
oilbase <- scales::dollar(preddate$oilprice)
fuelbase <- scales::dollar(preddate$fuelprice)

predictions_decade <-
    linearanalysis %>%
    filter(term == "oilprice") %>%
    select(halfdecade, data, models) %>%
    mutate(predictions = map(
        models,
        ~ broom::augment(.x,
            newdata = preddate,
            interval = "prediction"
        )
    )) %>%
    unnest(predictions) %>%
    select(halfdecade, .fitted, .lower, .upper) %>%
    inner_join(rsq_table)

predictions_decade %>%
    ggplot() +
    aes(y = halfdecade, x = .fitted) +
    geom_point(size = 3) +
    geom_vline(xintercept = preddate$fuelprice, lty = 2) +
    geom_errorbar(aes(xmin = .lower, xmax = .upper), width = .2) +
    annotate("label",
        preddate$fuelprice,
        1,
        label = glue::glue("Current\n{fuelbase} / gal")
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
        x = "Predicted fuel price (in $/gallon)",
        y = NULL,
        caption = "Error bars represent 95% prediction interval\nSource: Federal Reserve Bank, St. Louis (FRED)", # nolint
        title = glue::glue("What would fuel prices have been with oil at {oilbase} per barrel over the last {period} years?") # nolint
    ) +
    theme(plot.title.position = "plot")

ggsave("graphs/fuelprice_prediction.png", width = 8, height = 6)

models %>%
    unnest(data) %>%
    ggplot() +
    aes(x = oilprice, y = fuelprice, color = halfdecade) +
    geom_point(alpha = .2, show.legend = FALSE) +
    geom_smooth(method = "lm", color = "black", lty = 3) +
    facet_wrap(~halfdecade)

#
# inflation correction?
#

inflation <- get_index("CPIAUCSL") %>%
    mutate(annual_inflation = value / lag(value)) %>%
    mutate(year = year(date))

now <- lubridate::floor_date(tail(inflation, 1)$date - years(1) * 0:100,
                            unit = "month")

sorted_inflation <-
    inflation %>% filter(date %in% now)

last_inflation <- tail(sorted_inflation, 1)$value

predictions_decade %>%
    separate(halfdecade,
        sep = "-",
        c("lowyear", "highyear"),
        remove = FALSE
    ) %>%
    mutate(across(ends_with("year"), as.numeric)) %>%
    mutate(year = floor(mean(c(lowyear, highyear)))) %>%
    inner_join(., sorted_inflation, by = c("highyear" = "year")) %>%
    mutate(across(.fitted:.upper, ~ . * last_inflation / value)) %>%
    ggplot() +
    aes(y = halfdecade, x = .fitted) +
    geom_point(size = 3) +
    geom_vline(xintercept = preddate$fuelprice, lty = 2) +
    geom_errorbar(aes(xmin = .lower, xmax = .upper), width = .2) +
    annotate("label",
        preddate$fuelprice,
        .7,
        label = glue::glue("Current\n{fuelbase} / gal"),
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
        x = "Predicted fuel price (in $/gallon); inflation-corrected by CPIAUCSL", # nolint
        y = NULL,
        caption = "Error bars represent 95% prediction interval\nSource: Federal Reserve Bank, St. Louis (FRED)", # nolint
        title = glue::glue("What would fuel prices have been with oil at {oilbase} per barrel adjusted for inflation?") # nolint
    ) +
    theme(plot.title.position = "plot")

ggsave("graphs/fuelprice_prediction_withinflation.png", width = 8, height = 6)