
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

base_jan_2020 <- 259.127

info <-
    retrieve_data(indexes = c("APU0000703112", "CPIAUCSL"), "FRED") %>%
        pivot_wider(names_from = "index") %>% 
    janitor::clean_names() %>%
    arrange(date) %>%
    mutate(
        apu0000703112 = zoo::na.approx(apu0000703112, na.rm = FALSE),
        cpiaucsl = zoo::na.approx(cpiaucsl, na.rm = FALSE)
        ) %>%
    fill(apu0000703112, .direction = "down") %>%
    fill(cpiaucsl, .direction = "down") %>%
    mutate(beef = apu0000703112 / cpiaucsl * base_jan_2020) %>%
    drop_na()


ggplot(info, aes(x = date, y = beef)) +
    geom_point(shape = 1) + 
    scale_y_continuous(
        labels = scales::label_dollar(),
        # limits = c(0, NA)
    ) +
    scale_x_date(
        date_labels = "%Y",
        date_breaks = "2 years",
        limits = c(ymd(20100101), NA)
    ) +
    labs(
        x ="Date",
        y = "Beef Price Index (inflation adjusted to Jan 2020)",
    ) + 
    geom_vline(xintercept = c(ymd(20160120), ymd(20210120), ymd(20250120)), lty = 2)
