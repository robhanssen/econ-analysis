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


# find biggest change
spr_cleaned %>%
    filter(period > "2021-01-21") %>%
    summarize(
        max = max(value),
        min = min(value)
    ) %>%
    mutate(change = (min - max) / max)

xlims <- range(spr_cleaned %>% filter(period > "2009-01-01") %>% pull(period))

spr_g <-
    spr_cleaned %>%
    # filter(period > "2009-01-01") %>%
    ggplot(
        aes(x = period, y = value)
    ) +
    geom_point(shape = 1, size = .25) +
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-3),
        limits = c(0, NA),
        name = "Strategic petrol reserve (in millions of barrels)"
    ) +
    scale_x_date(
        name = ""
    ) +
    # geom_vline(
    #     xintercept = inaugdates, linetype = 2,
    #     color = "gray80"
    # ) +
    # coord_cartesian(xlim = xlims) +
    labs(
        title = "Strategic Petroleum Reserve",
        caption = "Source: US Energy Information Administration API"
    )

ggsave("graphs/strat_petrol_reserve.png",
    width = 8, height = 5,
    plot = spr_g
)

low_level <-
    spr_cleaned %>%
    filter(period > "2009-01-01") %>%
    slice_min(value) %>%
    pull(value)

spr_cleaned_old <-
    spr_cleaned %>% filter(period < "2009-01-01")

low_level_date <- as.Date(approx(spr_cleaned_old$value, spr_cleaned_old$period, xout = low_level)$y)

spr_g2 <-
    spr_g +
    geom_hline(yintercept = low_level, color = "red", linewidth = .25, linetype = "solid") +
    annotate(
        geom = "segment",
        x = low_level_date + years(2), y = low_level * .75, xend = low_level_date + weeks(13), yend = low_level * .98,
        arrow = arrow(length = unit(0.1, "inches"), angle = 25)
    ) +
    annotate(
        geom = "text",
        x = low_level_date + years(2), y = low_level * .75,
        hjust = -0.02, vjust = 1, label = format(low_level_date, format = "%B %e %Y")
    )

ggsave("graphs/strat_petrol_reserve_low_date.png",
    width = 8, height = 5,
    plot = spr_g2
)


spr_filer <- 
    spr_cleaned %>%
    filter(period >= "2026-04-01") %>%
    lm(value ~ period, data = .) %>% 
    broom::augment(newdata = tibble(period = seq.Date(from = as.Date("2026-04-01"), to = as.Date("2027-11-01"), by = "1 week"))) %>%
    select(period, value = .fitted) 

legal_min <- 252400
xover <- with(spr_filer, approx(value, period, xout = legal_min))
legal_date <- as.Date(with(spr_filer, xover$y, origin = "1970-01-01"))



spr_cleaned %>%
    filter(period > "2026-01-01") %>%
    ggplot(
        aes(x = period, y = value)
    ) +
    geom_point(shape = 21, size = 3)  +
    geom_line(data = spr_filer %>% filter(period < legal_date + months(1)), linetype = "dashed", color = "gray50") + 
    geom_hline(yintercept = 252400, color = "red", linewidth = .25, linetype = "solid") + 
    scale_y_continuous(
        labels = scales::label_number(scale = 1e-3),
    ) + 
    annotate(geom = "text", x = ymd(20260101), y = legal_min, color = "red", label = "SPR legal minimum", hjust = 0, vjust = -1) +
    annotate(geom = "point", x= legal_date, y = legal_min, color = "red", size = 3) +
    annotate(geom = "text", x = legal_date, y = xover$x, label = format(legal_date, format = "%b %e %Y"), hjust = 1, vjust = 2) +
    labs(
    x = "Date", y = "Strategic Petrol Reserve level (in MBB)"
    )

ggsave("dev/spr_min.png", height = 6, width = 8)