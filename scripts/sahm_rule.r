library(tidyverse)
library(patchwork)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "none"
    ))

source("functions.r")

cycles <-
    jsonlite::read_json("https://data.nber.org/data/cycles/business_cycle_dates.json", simplifyVector = TRUE) %>%
    mutate(across(everything(), ymd))

unrate <- get_index("UNRATE")

# defining constants
sahm_warning_limit <- 0.5
sahm_scale <- 5


sahm_g <-
    unrate %>%
    filter(date >= "1998-01-01") %>%
    mutate(
        three_month_av = zoo::rollmeanr(value, 3, fill = NA, na.rm = TRUE),
        twelve_month_min = zoo::rollapply(three_month_av, 12, min, fill = NA, align = "right"),
        sahm = three_month_av - twelve_month_min,
        sahm_warning = (sahm >= sahm_warning_limit)
    ) %>%
    ggplot(aes(date, value)) +
    geom_hline(
        yintercept = .5 * sahm_scale,
        linetype = "solid",
        color = "red",
        linewidth = 1.5,
        alpha = .1
    ) +
    geom_line(alpha = .7, linewidth = .5, aes(color = sahm_warning, group = 1)) +
    geom_line(
        aes(y = sahm_scale * sahm, color = sahm_warning, group = 2),
        linetype = 1, alpha = .5, # color = "gray50",
        linewidth = .1
    ) +
    geom_point(
        aes(y = sahm_scale * sahm, color = sahm_warning, alpha = sahm_warning),
        shape = 19, size = .5
    ) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray30")) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .333)) +
    scale_y_continuous(
        name = "U3 Unemployment rate (in %)",
        sec.axis = sec_axis(
            ~ . / (100 * sahm_scale),
            name = "Sahm Rule value",
            breaks = seq(0, 1, .005),
            labels = scales::label_percent()
        )
    ) +
    coord_cartesian(
        ylim = c(0, 15)
    ) +
    geom_vline(
        xintercept = cycles$peak, alpha = .2
    )

ggsave("graphs/sahm_rule_graph.png",
    width = 8, height = 5, plot = sahm_g
)
