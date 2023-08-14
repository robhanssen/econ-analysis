library(tidyverse)
library(quantmod)

rub_raw <- getSymbols("RUB=X", src = "yahoo", auto.assign = FALSE)

usdrub <- tibble(
    date = zoo::index(rub_raw),
    value = as.numeric(rub_raw$"RUB=X.Close")
) %>%
    filter(value > 10)


md <- usdrub %>%
    filter(date > ymd(20220701)) %>%
    lm(value ~ poly(date, 2), data = .) %>%
    broom::augment() %>%
    mutate(date = usdrub %>%
    filter(date > ymd(20220701)) %>% pull(date))


usdrub %>%
    filter(date > ymd(20200101)) %>%
    ggplot(aes(date, value)) +
    geom_point(alpha = .05) + 
    geom_line(aes(y = .fitted), data = md)