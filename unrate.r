library(tidyverse)
library(lubridate)

source("functions.r")

unrate <- retrieve_data("UNRATE", "FRED")

#
# monthly unemployment fitting since 2008
#

unrate2003m <-
    unrate %>%
    filter(date > ymd(20030101), date < ymd(20200101)) %>%
    mutate(time = (date - ymd(20080101)) / dmonths(1)) %>%
    filter(time > 0)

unrate_nls3 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate2003m
    )

model_2003 <- broom::augment(unrate_nls3,
    newdata = tibble(time = 1:(15 * 12))
) %>%
    mutate(date = ymd(20080101) + months(time))

broom::tidy(unrate_nls3)

#
# monthly unemployment fitting in 2000-2005
#
unrate200m <-
    unrate %>%
    filter(date > ymd(20000101), date < ymd(20060101)) %>%
    mutate(time = (date - ymd(20000101)) / dmonths(1)) %>%
    filter(time > 0)


unrate_nls4 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate200m
    )

broom::tidy(unrate_nls4)

model_200 <- broom::augment(unrate_nls4,
    newdata = tibble(time = 1:(12 * 12))
) %>%
    mutate(date = ymd(20000101) + months(time))


#
# monthly unemployment fitting in 1990-1995
#

unrate1990 <-
    unrate %>%
    filter(date > ymd(19900101), date < ymd(19950101)) %>%
    mutate(time = (date - ymd(19900101)) / dmonths(1)) %>%
    filter(time > 0)

unrate_nls5 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate1990
    )

model_1990 <- broom::augment(unrate_nls5,
    newdata = tibble(time = 1:(12 * 12))
) %>%
    mutate(date = ymd(19900101) + months(time))

broom::tidy(unrate_nls5)

#
# FULL GRAPHS
#


ggplot(data = unrate %>% filter(date > ymd(19900101))) +
    aes(date, value) +
    geom_point(aes(y = value), alpha = .1) +
    geom_line(data = model_200, aes(y = .fitted), lty = 2) +
    geom_line(data = model_2003, aes(y = .fitted), lty = 2) +
    geom_line(data = model_1990, aes(y = .fitted), lty = 2)