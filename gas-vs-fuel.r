library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

oilfuel <-
    inner_join(
        get_index("DCOILWTICO", "FRED") %>%
            mutate(value = value / 42) %>%
            rename(oilprice = "value"),
        get_index("GASREGW", "FRED") %>%
            rename(fuelprice = "value"),
        by = "date"
    ) %>%
    select(-index.x, -index.y)

decade_length <- 5
max_year <- max(year(oilfuel$date))

# oilfuel %>%
#     mutate(dec = 5*(year(date) %/% 5), dec = factor(dec)) %>%
#     ggplot + aes(oilprice, fuelprice, color = dec) + geom_point() + 
#     geom_smooth(method = "lm", se = F)


models <-
    oilfuel %>%
    mutate(
        year = year(date),
        halfdecade = decade_length * (year %/% decade_length),
        halfdecade_str = case_when(halfdecade + decade_length - 1 > max_year ~ paste0(halfdecade, "-", max_year),
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

slopes <- linearanalysis %>%
    filter(term == "oilprice") %>%
    mutate(min = estimate - 1.96 * std.error, max = estimate + 1.96 * std.error) %>%
    select(halfdecade, estimate, p.value, min, max)

intercpts <- linearanalysis %>%
    filter(term == "(Intercept)") %>%
    mutate(min = estimate - 1.96 * std.error, max = estimate + 1.96 * std.error) %>%
    select(halfdecade, estimate, p.value, min, max)


(intercpts %>%
    ggplot(aes(y = halfdecade, x = estimate)) +
    geom_point(alpha = .8, size = 3) +
    geom_errorbar(aes(xmin = min, xmax = max), width = .2) +
    expand_limits(x = 0) +
    labs(
        x = "Intercept fuelprice ~ oilprice",
        y = NULL
    )
) +
(slopes %>%
    ggplot(aes(y = halfdecade, x = estimate)) +
    geom_col(alpha = .8) +
    geom_errorbar(aes(xmin = min, xmax = max), width = .2) +
    labs(
        x = "Slope fuelprice ~ oilprice",
        y = NULL
    )
)

linearanalysis %>%
    mutate(preddate = map(models, broom::augment)) %>%
    unnest(preddate) %>%
    filter(oilprice > 0 ) %>%
    ggplot + aes(oilprice, .fitted, color = halfdecade) + geom_line()


preddate <- tail(oilfuel %>% arrange(date),1)

oilbase = scales::dollar(preddate$oilprice * 42)
fuelbase = scales::dollar(preddate$fuelprice)

linearanalysis %>%
    filter(term == "oilprice") %>%
    select(halfdecade, data, models) %>%
    mutate(predictions = map(models, ~broom::augment(.x, newdata = preddate, interval = "prediction"))) %>%
    unnest(predictions) %>%
    select(halfdecade, .fitted, .lower, .upper) %>%
    ggplot + aes(y = halfdecade, x = .fitted) + geom_point() + geom_vline(xintercept = preddate$fuelprice, lty = 2) + 
    geom_errorbar(aes(xmin = .lower, xmax = .upper), width = .2) + 
    annotate("label", preddate$fuelprice, 1, label = glue::glue("Current\n{fuelbase} / gal")) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(x = "Predicted fuel price (in $/gallon)",
         y = NULL,
         title = glue::glue("What would fuel prices have been with oil at {oilbase} per barrel over the 30 years?"))


ggsave("graphs/fuelprice_prediction.png", width = 8, height = 6)
