library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

eurusd <- retrieve_data("DEXUSEU", "FRED")

dates <- ymd(c(20020101, 20030826, 20080107, 20100101, 20101020))

eurusd %>% 
    drop_na() %>%
    ggplot + aes(date, value) + geom_point(size = 1) + 
    geom_vline(xintercept = dates, lty = 3) +
    geom_hline(yintercept = 1.0) + 
    coord_cartesian(xlim = c(ymd(20200101), NA_Date_))

