#
# Original articles:
# https://www.frbsf.org/economic-research/publications/economic-letter/2023/may/rise-and-fall-of-pandemic-excess-savings/
# https://www.frbsf.org/our-district/about/sf-fed-blog/excess-no-more-dwindling-pandemic-savings/
#

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

savings <-
    retrieve_data(indexes = c("PMSAVE"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    janitor::clean_names() %>%
    arrange(date) %>%
    mutate(pmsave = pmsave / 12) # correction of Seasonable Adjusted Annual Rates to Monthly

lin <-
    savings %>%
    filter(date %within% (ymd(20160101) %--% ymd(20191231))) %>%
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
            tibble(date = seq(ymd(20230101), ymd(20250101), by = "1 month"))
    )

eqdate <-
    approx(cumxs$.fitted, cumxs$date, xout = 0)$y %>%
    as.Date(., origin = "1970-01-01")

savings_g <-
    savings %>%
    filter(date >= ymd(20160101)) %>%
    ggplot(aes(date, pmsave)) +
    geom_point() +
    geom_line(alpha = .4) +
    geom_line(
        data = lin, aes(date, .fitted),
        color = "gray80", alpha = .8,
        linewidth = 1, lty = "dashed"
    ) +
    # geom_vline(xintercept = c(eqdate), color = "gray70") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(
        breaks = seq(0, 600, 100),
    ) +
    labs(
        x = "", y = "Personal savings rate (in B$)",
        caption = "Source: FRED PMSAVE"
    )

excess_savings_g <-
    excess_modeled %>%
    bind_rows(tibble(
        date = seq(ymd(20160101), ymd(20200101), by = "1 month")
    )) %>%
    arrange(date) %>%
    ggplot(aes(date, cum_excess)) +
    geom_line(
        data = lin, aes(date, .fitted),
        color = "white", alpha = .0,
        linewidth = .1, lty = "dashed"
    ) +
    geom_point() +
    geom_line(alpha = .4) +
    # geom_vline(xintercept = c(eqdate), color = "gray70") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(
        breaks = seq(0, 2400, 400),
    ) +
    labs(
        x = "", y = "Excess savings since Jan 2020 (in B$)",
    )

ggsave("dev/savings.png",
    width = 6, height = 4,
    plot = savings_g
)

p <- excess_savings_g / savings_g

ggsave("dev/savings_sum.png",
    width = 6, height = 8,
    plot = p
)

cpi <-
    retrieve_data("CPIAUCSL") %>%
    pivot_wider(names_from = "index") %>%
    janitor::clean_names() %>%
    arrange(date)

cpi_adj_pmsave <-
    left_join(savings, cpi, by = join_by(date)) %>%
    filter(date > ymd(20160101)) %>%
    mutate(pmsave_cpi = pmsave * (cpi %>% filter(date == ymd(20200101)) %>% pull(cpiaucsl)) / cpiaucsl)

cpi_adj_lin <-
    cpi_adj_pmsave %>%
    filter(date %within% (ymd(20160101) %--% ymd(20191231))) %>%
    lm(pmsave_cpi ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20160601), today(), by = "1 month"))
    )


excess_modeled_cpi <-
    inner_join(cpi_adj_pmsave, cpi_adj_lin) %>%
    filter(date > ymd(20200101)) %>%
    mutate(
        excess_saving = (pmsave_cpi - .fitted) / 1,
    ) %>%
    mutate(
        cum_excess = cumsum(excess_saving)
    )

cumxs_cpi <-
    excess_modeled_cpi %>%
    filter(date > ymd(20220201)) %>%
    lm(cum_excess ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20230101), ymd(20250101), by = "1 month"))
    )

savings_g_cpi <-
    cpi_adj_pmsave %>%
    filter(date >= ymd(20160101)) %>%
    ggplot(aes(date, pmsave_cpi)) +
    geom_point() +
    geom_line(alpha = .4) +
    geom_line(
        data = cpi_adj_lin, aes(date, .fitted),
        color = "gray80", alpha = .8,
        linewidth = 1, lty = "dashed"
    ) +
    # geom_vline(xintercept = c(eqdate), color = "gray70") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(
        breaks = seq(0, 600, 100),
    ) +
    labs(
        x = "", y = "Personal savings rate (in B$)",
        caption = "Source: FRED PMSAVE"
    )

excess_savings_g_cpi <-
    excess_modeled_cpi %>%
    bind_rows(tibble(
        date = seq(ymd(20160101), ymd(20200101), by = "1 month")
    )) %>%
    arrange(date) %>%
    ggplot(aes(date, cum_excess)) +
    geom_line(
        data = lin, aes(date, .fitted),
        color = "white", alpha = .0,
        linewidth = .1, lty = "dashed"
    ) +
    geom_point() +
    geom_line(alpha = .4) +
    # geom_vline(xintercept = c(eqdate), color = "gray70") +
    coord_cartesian(ylim = c(0, NA)) +
    scale_y_continuous(
        breaks = seq(0, 2400, 400),
    ) +
    labs(
        title = "Adjusted for CPI (2020 dollars)",
        x = "", y = "Excess savings since Jan 2020 (in B$)",
    )


p2 <- (excess_savings_g + excess_savings_g_cpi) / (savings_g + savings_g_cpi)

ggsave("dev/savings_sum_cpi.png",
    width = 12, height = 8,
    plot = p2
)

#
# by person
#
#

savingspop_dat <-
    retrieve_data(
        indexes = c("PMSAVE", "POPTOTUSA647NWDB", "CPIAUCSL"),
        "FRED"
    ) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    janitor::clean_names() %>%
    mutate(
        pop = zoo::na.approx(poptotusa647nwdb, na.rm = FALSE),
        cpi = zoo::na.approx(cpiaucsl, na.rm = FALSE),
        cpi_index = cpi[.$date == ymd(20160101)],
        pmsave = pmsave / 12
    ) %>%
    fill(pop, .direction = "downup") %>%
    fill(cpi, .direction = "downup") %>%
    mutate(
        pm_save_by_person = pmsave * 1e9 / pop,
        pm_save_by_person_cpi = pm_save_by_person * cpi_index / cpi
    )

savingspop_dat %>%
    filter(date >= ymd(20160101)) %>%
    ggplot(aes(date)) +
    geom_line(color = "darkgreen", aes(y = pm_save_by_person_cpi)) +
    # geom_line(color = "red", aes(y = pm_save_by_person)) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_x_date(date_minor_breaks = "1 year") +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = seq(0, 2000, 200),
    ) +
    labs(
        x = "", y = "Personal savings rate per person per month (in 2016 $)",
        caption = "Source: FRED PMSAVE, POPTOTUSA647NWDB, CPIAUCSL",
        title = "CPI-adjusted savings per person per month"
    )
