library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

data <- 
    bind_rows(
        get_index("COMPNFB"),
        get_index("OPHNFB")
    ) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    rename(productivity = "OPHNFB", wages = "COMPNFB") 

inx <- data %>%
    filter(date == ymd(19700401))

data %>%
    mutate(productivity_i = 100 *productivity / as.numeric(inx['productivity']),
          wages_i = 100 * wages / as.numeric(inx['wages'])
          ) %>%
    ggplot(aes(x = date)) + 
    geom_line(aes(y = wages_i), color = "red") + 
    geom_line(aes(y = productivity_i), color = "blue")