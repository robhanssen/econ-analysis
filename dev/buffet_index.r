
library(tidyverse)
library(patchwork)
library(lubridate)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.grid.minor = element_blank()
    ))

source("functions.r")

info <-
    retrieve_data(indexes = c("GDP", "WILL5000PR"), "FRED") %>%
    pivot_wider(names_from = "index") %>% 
    janitor::clean_names() %>%
    arrange(date) %>%
    mutate(
        gdp = zoo::na.approx(gdp, na.rm = FALSE),
        will5000pr = zoo::na.approx(will5000pr, na.rm = FALSE)
        ) %>%
    fill(gdp, .direction = "down") %>%
    fill(will5000pr, .direction = "down") %>%
    mutate(buffet = will5000pr / gdp) %>%
    drop_na()

sigma_lines <-
    quantile(info$buffet, probs = pnorm(-2:2), na.rm = TRUE)

lines <-
    info %>%
    filter(date>"1979-01-01", date < "1990-01-01") %>%
    lm(log(buffet) ~ date, data = .) %>%
    broom::augment(
        newdata = tibble(date = seq(ymd(19790101), today(), by = "6 months"))
    ) %>% mutate(.fitted = exp(.fitted))


info %>%
    ggplot(aes(x = date, y = buffet)) +
    geom_point(shape = 1, alpha = .1) + 
    scale_y_log10(
        # limits = c(0, 2.42)
    ) + 
    geom_hline(yintercept = sigma_lines, lty = 2)  +
    geom_line(
        data = lines,
        aes(y = .fitted),
        lty = 1, color = "red"
    )