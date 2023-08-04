library(tidyverse)

rub_raw <- quantmod::getSymbols("RUB=X", src = "yahoo", auto.assign = FALSE)

rub <-
    bind_cols(
        date = zoo::index(rub_raw),
        as_tibble(rub_raw)
    ) %>%
    janitor::clean_names()



ggplot(rub %>% filter(date > ymd(20220101)), 
    aes(x = date, y = rub_x_close)) + 
    geom_point()
