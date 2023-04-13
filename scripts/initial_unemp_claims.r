library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("functions.r")

initialclaims <- get_index("ICSA") %>%
    arrange(date) %>%
    mutate(month = month(date), year = year(date)) %>%
    rename(claims = value) %>%
    select(-index)

cutoff_date <- ymd(20140101)
base_years <- c(2016, 2017, 2018, 2019)

sigma_width <- 6
q0 <- pnorm(sigma_width / 2)

annot <-
    glue::glue(
        "{sigma_width}\U03C3 based on period\n",
        "full-year {min(base_years)}-{max(base_years)}"
    )

quants_average <-
    with(
        initialclaims,
        quantile(
            claims[year %in% base_years],
            c(q0, 1 - q0)
        )
    )

claimsplot <-
    initialclaims %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(date, claims) +
    geom_point(color = "gray30", alpha = .5, size = .6) +
    geom_hline(
        yintercept = quants_average,
        size = 2, alpha = .3, color = "gray50"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
        breaks = 100e3 * 0:100,
        labels = scales::label_number(
            scale = 1e-3,
            suffix = " K"
        )
    ) +
    coord_cartesian(ylim = c(0, 6e5)) +
    labs(
        x = "", y = "",
        title = "Weekly initial unemployment claims",
        caption = "Source: FRED St. Louis ICSA data\nLine shows 26-week rolling average" # nolint
    ) +
    annotate("text",
        x = cutoff_date + months(1),
        y = mean(quants_average), label = annot,
        hjust = 0, color = "gray50", alpha = .8,
        size = 3
    ) +
    geom_segment(
        aes(
            x = cutoff_date, xend = cutoff_date,
            y = min(quants_average) + 10000, yend = max(quants_average) - 10000
        ),
        color = "gray85", alpha = .3, size = 1,
    ) +
    geom_line(
        aes(y = zoo::rollmean(claims, 26, na.pad = TRUE, align = "center")),
        color = "gray50", alpha = .2, size = 3
    ) +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    )

normal_label <-
    glue::glue(
        "Normality based on period\n",
        "full-year {min(base_years)}-{max(base_years)}"
    )

qqinset <-
    initialclaims %>%
    filter(year %in% base_years) %>%
    ggplot() +
    aes(sample = claims) +
    geom_qq_line(alpha = .3) +
    stat_qq(alpha = .1, size = .5) +
    labs(x = "", y = "") +
    scale_y_continuous(labels = scales::label_number(
        scale = 1e-3,
        suffix = "K"
    )) +
    annotate("text",
        x = -3, y = 280000,
        label = normal_label, hjust = 0, size = 2
    ) +
    theme(
        plot.background = element_rect(fill = "transparent")
    )

# p <- claimsplot + inset_element(qqinset, .01, .6, .3, .99)

# ggsave("graphs/initial_unemp_claims.png", width = 8, height = 5, plot = p)

# modeling 12 weeks of data
data_12weeks <-
    initialclaims %>%
    slice_max(date, n = 12)

linmod_12weeks <-
    initialclaims %>%
    slice_max(date, n = 12) %>%
    lm(claims ~ date, data = .) %>%
    broom::augment(newdata = tibble(
        date = seq(min(data_12weeks$date),
            max(data_12weeks$date) + weeks(6),
            by = "1 week"
        )
    ))

p <-
    claimsplot +
    geom_line(
        data = linmod_12weeks,
        aes(x = date, y = .fitted),
        color = "gray50", alpha = .5
    ) +
    inset_element(qqinset, .01, .6, .3, .99)

ggsave("graphs/initial_unemp_claims.png", width = 8, height = 5, plot = p)
