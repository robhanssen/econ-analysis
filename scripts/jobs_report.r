library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

jobs <- retrieve_data("PAYEMS", "FRED") %>%
    mutate(
        year = year(date),
        month = month(date)
    ) %>%
    arrange(date) %>%
    mutate(value = value * 1000) %>%
    mutate(growth_month = value - lag(value)) %>%
    group_by(month) %>%
    mutate(growth_annual = value - lag(value)) %>%
    ungroup() %>%
    mutate(prezpd = cut(date, breaks = inaugdates))

pjobs <- jobs %>%
    filter(date > ymd(19810120)) %>%
    group_by(prezpd) %>%
    mutate(y = 1, mo = cumsum(y), gr = cumsum(growth_month)) %>%
    ungroup() %>%
    mutate(inaugdate = ymd(paste(prezpd))) %>%
    inner_join(presidentinfo)

labs <- pjobs %>%
    mutate(inaugdate = ymd(paste(prezpd))) %>%
    group_by(prezpd) %>%
    slice_max(mo, n = 1) %>%
    ungroup() %>%
    select(mo, gr, prezpd, inaugdate) %>%
    inner_join(presidentinfo)

last_date <- max(pjobs$date)
cpt <- glue::glue("Last updated on ", format(last_date, format = "%b %d, %Y"))

pjobs %>%
    ggplot() +
    aes(mo, gr, color = party, group = prezpd) +
    geom_line(show.legend = FALSE, size = 1) +
    scale_x_continuous(limit = c(0, 120)) +
    scale_y_continuous(labels = scales::number_format(
        scale = 1e-6,
        suffix = "M"
    )) +
    scale_color_manual(values = c("R" = "#ff0803", "D" = "#0000ff")) +
    geom_label(
        data = labs,
        aes(
            x = mo + 1,
            y = gr,
            label = president,
            color = NULL,
            hjust = 0
        ),
        show.legend = FALSE
    ) +
    labs(
        x = "Months in office",
        y = "Cumulative jobs growth",
        caption = cpt
    )

ggsave("graphs/jobsgrowth_by_president.png", width = 8, height = 6)

jobspop <- retrieve_data(indexes = c("PAYEMS", "POPTOTUSA647NWDB"), "FRED") %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    fill(POPTOTUSA647NWDB, .direction = "down") %>%
    drop_na() %>%
    mutate(PAYEMS = PAYEMS * 1000) %>%
    mutate(workingpop = PAYEMS / POPTOTUSA647NWDB)

date_breaks <-
    ymd(c(
        19680101,
        19750101,
        19810101,
        19920101,
        20020101,
        20100101,
        20210101,
        20300101
    ))

maxes <-
    jobspop %>%
    mutate(periods = cut(date, breaks = date_breaks)) %>%
    group_by(periods) %>%
    slice_max(workingpop, n = 1) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(workingpop_lab = scales::percent(workingpop, accuracy = .1))

(jobspop %>%
    filter(date > ymd(19600101)) %>%
    ggplot() +
    aes(date, PAYEMS) +
    geom_line() +
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = " M"),
    ) +
    labs(
        x = "Date",
        y = "Total working population"
    )
) +
    (jobspop %>%
        filter(date > ymd(19600101)) %>%
        ggplot() +
        aes(x = date, y = workingpop) +
        geom_line() +
        scale_y_continuous(
            labels = scales::label_percent(accuracy = 1),
            breaks = .05 * 0:20,
            limits = c(.25, .5)
        ) +
        geom_point(data = maxes, size = 3, alpha = .3) +
        ggrepel::geom_text_repel(
            data = maxes,
            aes(
                y = workingpop + .01,
                label = workingpop_lab
            ),
        ) +
        labs(
            x = "Date",
            y = "Working population as\nfraction of the total population"
        )
    ) +
    plot_annotation(
        caption =
            "Source: FRED St. Louis, PAYEMS and POPTOTUSA647NWDB"
    )

ggsave("graphs/labor-participation.png", width = 12, height = 6)