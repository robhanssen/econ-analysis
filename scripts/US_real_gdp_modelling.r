library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

gdp <- retrieve_data("GDPC1", "FRED")

date_cuts <-
    c(
        ymd(19000101),
        ymd(19600101),
        ymd(19700101),
        ymd(19910101),
        ymd(20000101),
        ymd(20020101),
        ymd(20070101),
        ymd(20090101),
        today(),
        today() + years(50)
    )

models <- gdp %>%
    mutate(period = cut(date, breaks = date_cuts)) %>%
    group_by(period) %>%
    nest() %>%
    mutate(gdp_model = map(
        data,
        function(tbl) {
            lm(log10(value) ~ date,
                data = tbl
            )
        }
    )) %>%
    filter(!period %in% c("1900-01-01", "2007-01-01", "2000-01-01"))

# extrapolate

futuredata <- tibble(date = seq(date_cuts[2],
    date_cuts[length(date_cuts) - 1],
    by = "month"
))

gdp_data <-
    gdp %>%
    mutate(period = cut(date, breaks = date_cuts))

growth_models <-
    models %>%
    mutate(params = map(gdp_model, ~ broom::tidy(.x))) %>%
    unnest(params) %>%
    filter(term == "date") %>%
    mutate(growth = 10 ^ (estimate * 365) - 1)


label <- tibble(
    date = ymd(paste(growth_models$period)),
    value = growth_models$growth
) %>%
    mutate(growth = scales::percent(value))

gdp_model_graph <-
    models %>%
    mutate(extradata = map(
        gdp_model,
        ~ broom::augment(.x,
            newdata = futuredata
        )
    )) %>%
    unnest(extradata) %>%
    mutate(gdp_real = 10 ^ (.fitted)) %>%
    mutate(period2 = cut(date, breaks = date_cuts)) %>%
    filter(period == period2) %>%
    left_join(gdp_data, by = c("date", "period")) %>%
    mutate(model_diff = value - gdp_real) %>%
    drop_na() %>%
    ggplot() +
    aes(x = date, y = value, color = period) +
    geom_line(aes(y = gdp_real)) +
    geom_point(show.legend = FALSE, alpha = .3) +
    geom_label(
        data = label,
        aes(
            x = date + years(4),
            y = 1000,
            label = growth,
            fill = factor(date)
        ),
        hjust = 0,
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none")

diff_gdp_model <-
    models %>%
    mutate(extradata = map(
        gdp_model,
        ~ broom::augment(.x,
            newdata = futuredata
        )
    )) %>%
    unnest(extradata) %>%
    mutate(gdp_real = 10 ^ (.fitted)) %>%
    mutate(period2 = cut(date, breaks = date_cuts)) %>%
    filter(period == period2) %>%
    left_join(gdp_data, by = c("date", "period")) %>%
    mutate(model_diff = value - gdp_real) %>%
    drop_na() %>%
    ggplot() +
    aes(x = date, y = model_diff, color = period) +
    geom_line(show.legend = FALSE, alpha = .3) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
    theme(legend.position = "none")

combined_graph <-
    (gdp_model_graph + labs(x = "", y = "GDP (in $B)") +
        theme(axis.text.x = element_blank())) /
        (diff_gdp_model + labs(y = "Deviation from model (in $B)"))

ggsave("graphs/real_gdp_modeled.png",
    width = 8, height = 8,
    plot = combined_graph
)