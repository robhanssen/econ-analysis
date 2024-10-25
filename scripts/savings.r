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

savingspop_dat <-
    retrieve_data(
        indexes = c("PMSAVE", "POP", "CPIAUCSL"),
        "FRED"
    ) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    janitor::clean_names() %>%
    mutate(
        pop = zoo::na.approx(pop, na.rm = FALSE) * 1e3,
        cpi = zoo::na.approx(cpiaucsl, na.rm = FALSE),
        cpi_index = cpi[.$date == ymd(20160101)],
        pmsave = pmsave / 12 * 1e9
    ) %>%
    fill(pop, .direction = "downup") %>%
    fill(cpi, .direction = "downup") %>%
    mutate(
        pm_save_by_person = pmsave / pop,
        pm_save_by_person_cpi = pm_save_by_person * cpi_index / cpi
    )

level2020 <-
    with(
        savingspop_dat,
        pm_save_by_person_cpi[date == ymd(20200101)]
    )

savingspop_dat <-
    savingspop_dat %>%
    mutate(level2020 = case_when(
        date < ymd(20200101) ~ NA_real_,
        TRUE ~ level2020
    ))


savings_cpi_g <-
    savingspop_dat %>%
    filter(date >= ymd(20160101)) %>%
    ggplot(aes(date)) +
    geom_line(color = "gray70", aes(y = pm_save_by_person_cpi)) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_x_date(date_minor_breaks = "1 year") +
    scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = seq(0, 8000, 200),
    ) +
    labs(
        x = "", y = "Personal savings rate\nper person per month (in 2016 $)",
        caption = "Source: FRED PMSAVE, POP, CPIAUCSL",
        title = "CPI-adjusted savings per person per month"
    )

line_cpi <-
    savingspop_dat %>%
    filter(date %within% (ymd(20160101) %--% ymd(20181231))) %>%
    lm(pm_save_by_person_cpi ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20200101), today(), by = "1 month"))
    )

extra_sav <-
    inner_join(savingspop_dat, line_cpi) %>%
    arrange(date) %>%
    filter(date >= ymd(20200101)) %>%
    mutate(
        diff_savings = pm_save_by_person_cpi - .fitted,
        cum_saving = cumsum(diff_savings)
    ) %>%
    select(date, diff_savings, cum_saving)

pers_savings_cpi_g <-
    savings_cpi_g +
    geom_line(
        data = line_cpi, aes(y = .fitted),
        lty = 2, color = "gray60"
    ) #+
    # geom_line(aes(y = level2020),
    #     lty = 2, color = "gray60"
    # )

extrpolt <- extra_sav %>%
    filter(date > ymd(20220601)) %>%
    lm(cum_saving ~ date, data = .) %>%
    broom::augment(
        newdata =
            tibble(date = seq(ymd(20220601), ymd(20250101), "1 month"))
    )

nr <-
    extrpolt %>%
    filter(.fitted > 0) %>%
    nrow() + 10

xdate <- with(
    extrpolt,
    approx(.fitted, date, xout = 0)$y
) %>% as.Date(., origin = "1970-01-10")


surplus_pers_savings_g <-
    extra_sav %>%
    ggplot(aes(x = date, y = cum_saving)) +
    geom_point() +
    geom_line(
        data = extrpolt[1:nr, ],
        aes(y = .fitted),
        alpha = .25,
        color = "gray70",
        linewidth = 2
    ) +
    annotate("text",
        x = xdate, y = 50, hjust = 1,
        label = format(xdate, format = "%b %d, %Y")
    ) +
    coord_cartesian(xlim = c(ymd(20160101), today())) +
    labs(
        x = "", y = "Cumulative savings\nsince Jan 1 2020",
        subtitle = "Savings per person, CPI-adjusted in 2016 dollars"
    )


g <- surplus_pers_savings_g / pers_savings_cpi_g + plot_layout(heights = c(1,2))

ggsave("graphs/pers-savings-cpi.png", height = 9, width = 8,
    plot = g)