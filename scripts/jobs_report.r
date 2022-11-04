library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

jobs <- retrieve_data("PAYEMS", "FRED") %>%
    mutate(year = year(date),
            month = month(date)) %>%
    arrange(date) %>%
    mutate(value = value  * 1000) %>%
    mutate(growth_month = value - lag(value)) %>%
    group_by(month) %>%
    mutate(growth_annual = value - lag(value)) %>%
    ungroup() %>%
    mutate(prezpd = cut(date, breaks = inaugdates))


View(jobs)

jobs %>%
    filter(date > ymd(19791231)) %>%
    ggplot + 
    aes(date, growth_month) + 
    geom_point() + 
    geom_vline(xintercept = inaugdates, alpha = .5) + 
    scale_y_continuous(limits = c(-1e3, 1e3))


jobs %>%
    filter(date > ymd(19791231)) %>%
    ggplot + 
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
    mutate(inaugdate = ymd(paste(prezpd))) %>% #select(prezpd, inaugdate)
    group_by(prezpd) %>%
    slice_max(mo, n= 1) %>%
    ungroup() %>%
    select(mo, gr, prezpd, inaugdate) %>%
    inner_join(presidentinfo)

pjobs %>%
    ggplot + 
    aes(mo, gr, color = party, group = prezpd) + 
    geom_line(show.legend = FALSE, size = 1) + 
    scale_x_continuous(limit = c(0, 120)) +
    scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
    scale_fill_manual(values = c("R" = "red", "D" = "darkblue")) +
    geom_label(data = labs, aes(x = mo, y = gr, label = president, color = NULL),
                show.legend = FALSE) + 
    labs(x = "Months in office",
        y = "Cumulative jobs growth")

ggsave("graphs/jobsgrowth_by_president.png", width = 8, height = 6)