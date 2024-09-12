library(tidyverse)
library(lubridate)
library(patchwork)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")

jobgrowth <- retrieve_data(c("PAYEMS")) %>%
    arrange(date) %>%
    mutate(month = month(date)) %>%
    mutate(jobgrowth = value - lag(value)) %>%
    ungroup()

jobgrowth_label <-
    jobgrowth %>%
    group_by(index) %>%
    slice_max(date, n = 1) %>%
    ungroup() %>%
    mutate(
        inflation = scales::number(jobgrowth),
        label = paste0(format(date, format = "%b %Y"), "\n", index, "\n", jobgrowth, "K")
    )

cutoff_date <- ymd(20100101)

job_g <-
    jobgrowth %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(date, jobgrowth) +
    ggrepel::geom_label_repel(
        data = jobgrowth_label,
        aes(
            x = date,
            y = jobgrowth,
            label = label
        ),
        hjust = "center",
        size = 2
    ) +
    geom_point(shape = 1, size = .5, alpha = .5) +
    scale_y_continuous(
        labels = scales::number_format(),
        expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(-1000, 1000)) +
    scale_x_date(
        date_breaks = "3 years",
        date_labels = "%Y",
        limits = c(as.Date(NA), today() + years(1))
    ) +
    labs(
        x = "",
        y = "Monthly job growth (in 1,000s)",
        caption = paste0(
            "Source: FRED PAYEMS\n",
            "Density plot shows monthly job growth from 2010-2019, with 5% and 95% quantile errorbars"
        )
    ) +
    geom_point(
        data = jobgrowth_label,
        aes(
            x = date,
            y = jobgrowth
        )
    ) +
    theme(
        legend.position = "none"
    )

job_dens <-
    jobgrowth %>%
    filter(year(date) %in% 2010:2019) %>%
    ggplot(aes(y = jobgrowth)) +
    geom_density(fill = "gray70", alpha = .4, color = "gray90") +
    scale_y_continuous(
        labels = scales::label_number(),
        expand = c(0, 0)
    ) +
    geom_errorbar(
        aes(
            x = 0,
            ymin = quantile(jobgrowth, .05),
            ymax = quantile(jobgrowth, .95)
        ),
        width = 1e-5,
        inherit.aes = FALSE
    ) +
    coord_cartesian(ylim = c(-1000, 1000)) +
    theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank()
    )

tot_jobs_g <-
    job_g + job_dens +
    patchwork::plot_layout(widths = c(9, 1))

ggsave("graphs/jobgrowth_density.png",
    width = 8, height = 6,
    plot = tot_jobs_g
)
