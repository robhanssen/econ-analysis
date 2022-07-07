library(tidyverse)
library(lubridate)
library(patchwork)

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

unrate_nls2008 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate2003m
    )

model_2003 <- broom::augment(unrate_nls2008,
    newdata = tibble(time = 1:(15 * 22))
) %>%
    mutate(date = ymd(20080101) + months(time))

broom::tidy(unrate_nls2008)

#
# monthly unemployment fitting in 2000-2005
#
unrate200m <-
    unrate %>%
    filter(date > ymd(20000101), date < ymd(20060101)) %>%
    mutate(time = (date - ymd(20000101)) / dmonths(1)) %>%
    filter(time > 0)


unrate_nls2000 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate200m
    )

broom::tidy(unrate_nls2000)

model_200 <- broom::augment(unrate_nls2000,
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

unrate_nls1990 <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate1990
    )

model_1990 <- broom::augment(unrate_nls1990,
    newdata = tibble(time = 1:(22 * 12))
) %>%
    mutate(date = ymd(19900101) + months(time))

broom::tidy(unrate_nls1990)

#
# monthly unemployment fitting in 1990-2000
#


unrate1990a <-
    unrate %>%
    filter(date > ymd(19900101), date < ymd(20000101)) %>%
    mutate(time = (date - ymd(19900101)) / dmonths(1)) %>%
    filter(time > 0)

unrate_1990a <-
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = unrate1990a
    )

model_1990a <- broom::augment(unrate_1990a,
    newdata = tibble(time = 1:(12 * 12))
) %>%
    mutate(date = ymd(19900101) + months(time))

broom::tidy(unrate_1990a)

#
# FULL GRAPHS
#

unemp <-
    ggplot(data = unrate %>% filter(date > ymd(19900101))) +
    aes(date, value) +
    geom_point(aes(y = value), alpha = .1) +
    geom_line(data = model_200, aes(y = .fitted), lty = 2) +
    geom_line(data = model_2003, aes(y = .fitted), lty = 2) +
    geom_line(data = model_1990a, aes(y = .fitted), lty = 2) + 
    scale_y_continuous(limits = c(0, NA)) + 
    labs(x = "Date",
        y = "Unemployment rate (in %)") +
    theme_light()

#
# model conclusions
#

extract_value <- function(mod) {
    mod %>% 
        broom::tidy() %>% 
        filter(term == "a0") %>% pull(estimate)
}

sumry <- tibble(
            year = c("2010-2020", "2000-2005", "1990-1995"),
            limit_value = purrr::map_dbl(list(unrate_nls2008, unrate_nls2000, unrate_1990a), ~extract_value(.x))
    )

limit <-
    sumry %>% 
    ggplot +
    aes(factor(year), limit_value) +
    geom_col() + 
    theme_light() + 
    labs(x = "Time period",
         y = "Unemployment limit value (in %)")

p <- unemp + limit

ggsave("graphs/unemployment-analysis.png", plot = p, width = 12, height = 6)