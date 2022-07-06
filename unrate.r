library(tidyverse)
library(lubridate)

source("functions.r")

unrate <- retrieve_data("UNRATE", "FRED")

ggplot(unrate %>% filter(date > ymd(20120101))) +
  aes(date, value) + 
  geom_point()


ggplot(unrate) +
  aes(date, value) + 
  geom_point()


unrate2010 <-
    unrate %>% filter(date > ymd(20100101), date < ymd(20200101))

unrate_mod <- lm(log10(value) ~ date, data = unrate2010)

broom::augment(unrate_mod, newdata = tibble(date = seq(ymd(20100101), today(), by = "month"))) %>%
    mutate(.fitted = 10^.fitted) %>%
    ggplot + aes(date, .fitted) + geom_line() + 
    geom_point(data = unrate2010, aes(y = value))

unrate2010_undated <- 
    unrate2010 %>% mutate(time = (date - ymd(20100101))/dyears(1))

unrate_nls <- 
    nls(value ~ a0 + a1 * 10^(-time / a2), start = list(a0 = 2, a1 = 5, a2 = 5), data = unrate2010_undated)


broom::augment(unrate_nls, newdata = tibble(time = 1:200)) %>%
    mutate(date = ymd(20100110) + years(time)) %>%
    # mutate(.fitted = 10^.fitted) %>%
    ggplot + aes(date, .fitted) + geom_line() + 
    geom_point(data = unrate %>% filter(date > ymd(20000101)), aes(y = value))





unrate2003 <-
    unrate %>% filter(date > ymd(20030101), date < ymd(20200101)) %>%
    mutate(time = (date - ymd(20070101))/dyears(1)) %>%
    filter(time > 0)

# unrate2003 %>%
#     ggplot +
#   aes(date, value) + 
#   geom_point()

# write_csv(unrate2003, "uni2.csv")

unrate_nls2 <- 
    nls(value ~ a0+ a1 * exp(-1/a3*(log(time) - a2)^2), start = list(a0 = 4, a1 = 15, a2 = 1, a3=1 ), data = unrate2003)

broom::augment(unrate_nls2, newdata = tibble(time = 1:15)) %>%
    mutate(date = ymd(20070101) + years(time)) %>%
    # mutate(.fitted = 10^.fitted) %>%
    ggplot + aes(date, .fitted) + geom_line() + 
    geom_point(data = unrate %>% filter(date > ymd(20000101)), aes(y = value))

broom::tidy(unrate_nls2)
