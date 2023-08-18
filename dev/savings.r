#
# Original articles:
# https://www.frbsf.org/economic-research/publications/economic-letter/2023/may/rise-and-fall-of-pandemic-excess-savings/
# https://www.frbsf.org/our-district/about/sf-fed-blog/excess-no-more-dwindling-pandemic-savings/
#

library(tidyverse)
library(lubridate)
theme_set(theme_light() +
    theme(
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")

savings <-
    retrieve_data(indexes = c("PMSAVE"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    janitor::clean_names() %>%
    arrange(date) %>%
    mutate(pmsave = pmsave)

lin <-
    savings %>%
    filter(date %within% (ymd(20160601) %--% ymd(20200101))) %>%
    lm(pmsave ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20160601), today(), by = "1 month"))
    )

excess_modeled <-
    inner_join(savings, lin) %>%
    filter(date > ymd(20200101)) %>%
    mutate(
        excess_saving = (pmsave - .fitted) / 1,
    ) %>%
    mutate(
        cum_excess = cumsum(excess_saving)
    )

cumxs <-
    excess_modeled %>%
    filter(date > ymd(20220201)) %>%
    lm(cum_excess ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20230101), ymd(20300101), by = "1 month"))
    )

eqdate <-
    approx(cumxs$.fitted, cumxs$date, xout = 0)$y %>%
    as.Date(., origin = "1970-01-01")

savings %>%
    filter(date > ymd(20160101)) %>%
    ggplot(aes(date, pmsave)) +
    geom_point() +
    geom_line(alpha = .4) +
    geom_line(
        data = lin, aes(date, .fitted),
        color = "gray80", alpha = .8,
        linewidth = 1, lty = "dashed"
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 6000, 1000),
    ) +
    # geom_point(data = excess_modeled, aes(date, cum_excess), color = "gray70") +
    geom_vline(xintercept = c(eqdate), color = "gray70") +
    labs(
        x = "", y = "Personal saving rate (in B$)",
        caption = "Source: FRED PMSAVE"
    )

ggsave("dev/savings.png", width = 6, height = 4)
