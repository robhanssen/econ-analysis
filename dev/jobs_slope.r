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

pd_df <-
    bind_rows(
        crossing(president = "Ronald Reagan", mo = 75:95),
        crossing(president = "Bill Clinton", mo = 40:80),
        crossing(president = "Barack Obama", mo = 50:95),
        crossing(president = "George H. W. Bush", mo = 1:13),
        crossing(president = "George W. Bush", mo = 45:65),
        crossing(president = "Donald Trump", mo = 1:25),
        crossing(president = "Joe Biden", mo = 22:29)
    )


modeled <-
    pjobs %>%
    inner_join(pd_df, by = c("president", "mo")) %>%
    select(mo, gr, prezpd, president) %>%
    group_by(president) %>%
    nest() %>%
    mutate(
        pd = map(data, ~ lm(gr ~ mo, data = .x)),
        dats = map(pd, broom::tidy),
        preds = map(pd, broom::augment)
    )

std_err_mult <- 3

g1 <-
    modeled %>%
    unnest(dats) %>%
    filter(term == "mo") %>%
    select(president, estimate, std.error) %>%
    ggplot(aes(x = estimate, y = fct_reorder(president, estimate))) +
    geom_col(width = .75, alpha = .5) +
    geom_errorbar(
        aes(
            xmin = estimate - 2 * std.error,
            xmax = estimate + 2 * std.error
        ),
        width = .3
    ) +
    scale_x_continuous(labels = scales::label_number()) +
    labs(
        x = "Modeled job growth per month", 
        y = "",
        caption = glue::glue(
            "Errorbars range \U00B1 ",
            std_err_mult,
            " * standard error"
        )
    )

preds <-
    modeled %>%
    unnest(preds) %>%
    select(president, mo, gr = .fitted)

g2 <-
    pjobs %>%
    ggplot() +
    aes(mo, gr, color = party, group = prezpd) +
    geom_line(show.legend = FALSE, linewidth = .3) +
    scale_x_continuous(limit = c(0, 120)) +
    scale_y_continuous(labels = scales::number_format(
        scale = 1e-6,
        suffix = "M"
    )) +
    scale_color_manual(values = c("R" = "#ff0803", "D" = "#0000ff")) +
    geom_line(
        data = preds,
        aes(mo, gr, group = president, color = NULL),
        linetype = 1,
        linewidth = 1.8,
        color = "gray60",
        alpha = .5
    ) +
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


ggsave("dev/jobs_growth.png",
    width = 12, height = 6,
    plot = g2 + g1 +
        plot_layout(widths = c(2, 1)) +
        plot_annotation(
            title = "Monthly job growth in stable growing periods by president"
        )
)
