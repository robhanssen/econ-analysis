library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

oil_data <-
    retrieve_data(c("DCOILWTICO", "CPIAUCSL")) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    fill(CPIAUCSL, .direction = "down") %>%
    mutate(oil_inflation = DCOILWTICO / CPIAUCSL) %>%
    drop_na(DCOILWTICO)

av_line <-
    oil_data %>%
    filter(date < ymd(20040101)) %>%
    summarize(x = mean(oil_inflation)) %>%
    pull(x)

fair_price <-
    oil_data %>%
    mutate(fair_price = av_line * CPIAUCSL)


xdates <- inner_join(oil_data, fair_price) %>%
    mutate(dif = DCOILWTICO - fair_price) %>%
    filter(year(date) %in% 2004:2005) %>%
    lm(dif ~ date, data = .) %>%
    augment(
        newdata = tibble(
            date = seq(ymd(20000101), ymd(20060101), by = "1 month")
        ),
        interval = "prediction"
    )

xdate <- map_dbl(
    c(".upper", ".lower"),
    ~ approx(xdates[[.x]], xdates$date, xout = 0)$y
) %>% as.Date(origin = ymd("1970-01-01"))

oil_fit <- oil_data %>%
    filter(year(date) %in% 2004:2005) %>%
    lm(DCOILWTICO ~ date, data = .) %>%
    augment(
        newdata = tibble(
            date = seq(ymd(20020101), ymd(20070101), by = "1 month")
        ),
        interval = "prediction"
    )

oil_data %>%
    ggplot(aes(x = date, y = DCOILWTICO)) +
    geom_vline(xintercept = presidentinfo$inaugdate, 
        color = "gray70", alpha = .1, linewidth = 2) +
    geom_point(shape = 1, alpha = .1, size = .3) +
    geom_line(data = fair_price, aes(y = fair_price)) +
    geom_line(data = oil_fit, aes(y = .upper), alpha = .8, color = "gray40") +
    geom_line(data = oil_fit, aes(y = .lower), alpha = .8, color = "gray40") +
    geom_line(data = oil_fit, aes(y = .fitted), lty = 2) +
    annotate("text",
        x = ymd(20110101), y = 25,
        hjust = 0, label = "Oil price model\nadjusted by inflation"
    ) +
    annotate("text",
        x = ymd(20110101), y = 125,
        hjust = 0, label = "Oil price\n(Actual)"
    ) +
    annotate("label",
        x = xdate + weeks(13), y = c(15, 25),
        label = format(xdate, format = "%b %d, %Y"), hjust = 0
    ) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "Oil price (West Texas Crude)")

ggsave("graphs/oilprice_comparison.png", width = 8, height = 5)
