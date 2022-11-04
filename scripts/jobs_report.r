library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

jobs <- retrieve_data("PAYEMS", "FRED") %>%
    mutate(
        year = year(date),
        month = month(date)
    ) %>%
    arrange(date) %>%
    mutate(value = value * 1000) %>%
    mutate(growth_month = value - lag(value)) %>%
    group_by(month) %>%
    mutate(growth_annual = value - lag(value)) %>%
    ungroup() %>%
    mutate(prezpd = cut(date, breaks = inaugdates))


View(jobs)

jobs %>%
    filter(date > ymd(19791231)) %>%
    ggplot() +
    aes(date, growth_month) +
    geom_point() +
    geom_vline(xintercept = inaugdates, alpha = .5) +
    scale_y_continuous(limits = c(-1e3, 1e3))


jobs %>%
    filter(date > ymd(19791231)) %>%
    ggplot() +
    aes(date, growth_annual) +
    geom_point() +
    geom_vline(xintercept = inaugdates, alpha = .5) +
    scale_y_continuous(limits = c(-1e4, 1e4))


pjobs <- jobs %>%
    filter(date > ymd(19810120)) %>%
    group_by(prezpd) %>%
    mutate(y = 1, mo = cumsum(y), gr = cumsum(growth_month)) %>%
    ungroup() %>%
    mutate(inaugdate = ymd(paste(prezpd))) %>%
    inner_join(presidentinfo)

labs <- pjobs %>%
    mutate(inaugdate = ymd(paste(prezpd))) %>%
    group_by(prezpd) %>%
    slice_max(mo, n = 1) %>%
    ungroup() %>%
    select(mo, gr, prezpd, inaugdate) %>%
    inner_join(presidentinfo)

last_date <- max(pjobs$date)
cpt <- glue::glue("Last updated on ", format(last_date, format = "%b %d, %Y"))

pjobs %>% 
    ggplot() +
    aes(mo, gr, color = party, group = prezpd) +
    geom_line(show.legend = FALSE, size = 1) +
    scale_x_continuous(limit = c(0, 120)) +
    scale_y_continuous(labels = scales::number_format(
        scale = 1e-6,
        suffix = "M"
    )) +
    scale_color_manual(values = c("R" = "#ff0803", "D" = "#0000ff")) +
    geom_label(
        data = labs, aes(x = mo + 1, y = gr, label = president, color = NULL, hjust = 0),
        show.legend = FALSE
    ) +
    labs(
        x = "Months in office",
        y = "Cumulative jobs growth",
        caption = cpt
    )

ggsave("graphs/jobsgrowth_by_president.png", width = 8, height = 6)