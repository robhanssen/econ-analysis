library(tidyverse)
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

api <-
    glue::glue(
        "https://api.eia.gov/v2/petroleum/sum/sndw/data/?",
        "frequency=weekly&",
        "data[0]=value&facets[series][]=WCSSTUS1&sort[0][column]=period&",
        "sort[0][direction]=desc&",
        "offset=0&",
        # "length=5000&",
        "api_key={spr_api_key}"
    )

spr_raw <- jsonlite::read_json(api)

spr_cleaned <-
    as_tibble(spr_raw$response) %>%
    unnest_wider(data) %>%
    janitor::clean_names() %>%
    mutate(
        period = ymd(period),
        value = as.numeric(value)
    ) %>%
    select(period, value, units) %>%
    arrange(period)

spr_filter <-
    spr_cleaned %>%
    filter(period >= "2026-04-21") %>%
    lm(value ~ period, data = .) %>%
    broom::augment(
        newdata = tibble(period = seq.Date(from = as.Date("2026-04-01"), to = as.Date("2027-11-01"), by = "1 week")),
        interval = "prediction"
    ) %>%
    select(period, value = .fitted, .lower, .upper)

legal_min <- c("SPR Legal Minimum" = 252400, "SPR Collapse Risk" = 150000)
xover <- with(spr_filter, approx(value, period, xout = legal_min))
legal_date <- as.Date(with(spr_filter, xover$y, origin = "1970-01-01"))

spr_min_g <- 
    spr_cleaned %>%
    filter(period > "2026-01-01") %>%
    ggplot(
        aes(x = period, y = value)
    ) +
    geom_ribbon(
        data = spr_filter %>% filter(period < max(legal_date) + months(1)),
        aes(ymin = .lower, ymax = .upper),
        fill = "gray80", alpha = 0.2
    ) +
    geom_point(shape = 21, size = 3) +
    geom_line(
        data = spr_filter %>% filter(period < max(legal_date) + months(1)),
        linetype = "dashed", color = "gray50"
    ) +
    geom_hline(yintercept = legal_min, color = "red", linewidth = .25, linetype = "solid") +
    scale_y_continuous(
        breaks = 1e3 * seq(100, 500, 50),
        labels = scales::label_number(scale = 1e-3)
    ) +
    annotate(
        geom = "text", x = ymd(20260101), y = legal_min,
        color = "red", label = names(legal_min), hjust = 0, vjust = -1
    ) +
    annotate(geom = "point", x = legal_date, y = legal_min, color = "red", size = 3) +
    annotate(
        geom = "text", x = legal_date, y = xover$x,
        label = format(legal_date, format = "%b %e %Y"), hjust = 1, vjust = 2
    ) +
    labs(
        x = "Date", y = "Strategic Petrol Reserve level (in MBB)"
    )

ggsave("graphs/spr_minimum_levels.png", height = 6, width = 8, plot = spr_min_g)
