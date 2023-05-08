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
    geom_line(show.legend = FALSE, linewidth = 1) +
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

jobspop_dat <-
    retrieve_data(indexes = c("PAYEMS", "POPTOTUSA647NWDB"), "FRED") %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    fill(POPTOTUSA647NWDB, .direction = "down") %>%
    drop_na() %>%
    mutate(PAYEMS = PAYEMS * 1000) %>%
    mutate(workingpop = PAYEMS / POPTOTUSA647NWDB)

jobspop <-
    full_join(presidentinfo, jobspop_dat, by = c("inaugdate" = "date")) %>%
    arrange(inaugdate) %>%
    select(-floordate) %>%
    fill(c("president", "party")) %>%
    drop_na(workingpop) %>%
    rename(date = inaugdate)

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
    aes(date, PAYEMS, color = party, group = TRUE) +
    geom_line(show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = " M"),
    ) +
    scale_color_manual(values = c("R" = "#ff0803", "D" = "#0000ff")) +
    labs(
        x = "Date",
        y = "Total working population"
    )
) +
    (jobspop %>%
        filter(date > ymd(19600101)) %>%
        ggplot() +
        aes(x = date, y = workingpop, color = party, group = TRUE) +
        geom_line(show.legend = FALSE) +
        scale_y_continuous(
            labels = scales::label_percent(accuracy = 1),
            breaks = .05 * 0:20,
            limits = c(.25, .5)
        ) +
        geom_point(data = maxes, size = 3, alpha = .3, show.legend = FALSE) +
        ggrepel::geom_text_repel(
            data = maxes,
            aes(
                y = workingpop + .01,
                label = workingpop_lab
            ), show.legend = FALSE
        ) +
        scale_color_manual(values = c("R" = "#ff0803", "D" = "#0000ff")) +
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

#
# post-COVID19 job recovery modeling
#

last6months <- jobs %>%
    slice_max(date, n = 6)

sixmonth_pred <-
    last6months %>%
    lm(value ~ date, data = .) %>%
    broom::augment(newdata = tibble(
        date = seq(min(last6months$date) %m-% months(6),
            max(last6months$date) %m+% months(6),
            by = "1 month"
        )
    ))

start_date <- ymd(20120101)
end_date <- ymd(20191201)
plot_start_date <- ymd(20140101)

fitted_jobs <- jobs %>%
    filter(date > start_date, date < end_date) %>%
    lm(value ~ date, data = .) %>%
    broom::augment(newdata = tibble(
        date = seq(start_date, today(), by = "1 month")
    ))

missing_jobs <-
    inner_join(fitted_jobs, last6months) %>%
    mutate(diff = .fitted - value) %>%
    select(date, diff, value, .fitted) %>%
    summarize(
        date = median(date),
        across(diff:.fitted, mean)
    ) %>%
    mutate(
        x = date,
        y = min(value, .fitted) + diff / 2,
        label = paste0(round(diff / 1e6, 2), " M")
    )

recov_plot <-
    ggplot(data = fitted_jobs) +
    geom_text(
        data = missing_jobs,
        size = 2,
        aes(x = x, y = y,
            label = glue::glue("{label} jobs\nare \"missing\""))
    ) +
    geom_line(aes(x = date, y = .fitted),
        linewidth = 1, alpha = .3,
        color = "gray50"
    ) +
    geom_line(aes(x = date, y = .fitted),
        data = sixmonth_pred,
        linewidth = 1, alpha = .3,
        color = "gray50"
    ) +
    geom_point(aes(x = date, y = value),
        data = jobs %>% filter(date > start_date),
        size = .5, shape = 1, alpha = .3
    ) +
    geom_point(aes(x = date, y = value),
        data = last6months,
        size = .5, alpha = 1,
        color = "black"
    ) +
    scale_y_continuous(labels = scales::label_number(
        scale = 1e-6,
        suffix = " M"
    )) +
    scale_x_date(date_label = "%Y", date_breaks = "2 year") +
    labs(
        title = "Post-COVID19 job recovery compared to 2012-2019 job growth",
        x = "", y = "Number of jobs (non-farm)",
        caption = "Source: FRED PAYEMS"
    ) +
    coord_cartesian(xlim = c(plot_start_date, NA)) +
    theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
    )

ggsave("graphs/jobs_recovery.png",
    width = 6, height = 4,
    plot = recov_plot
)
