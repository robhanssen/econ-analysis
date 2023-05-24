library(tidyverse)
library(lubridate)
theme_set(theme_light() +
    theme(plot.title.position = "plot"))

source("functions.r")

birthrate <-
    retrieve_data(indexes = c("SPDYNCBRTINUSA"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    arrange(date) %>%
    rename(birthrate = "SPDYNCBRTINUSA") %>%
    filter(date >= ymd(19900101))

recession_2008 <-
    tibble(
        x = ymd("20071201", "20090701", "20090701", "20071201"),
        y = c(0, 0, 20, 20)
    )


birthrate_model_old <-
    birthrate %>%
    filter(date <= ymd(19970101)) %>%
    lm(birthrate ~ date, data = .) %>%
    broom::augment()

birthrate_model_flat <-
    birthrate %>%
    filter(date <= ymd(20070101), date >= ymd(19970101)) %>%
    lm(birthrate ~ date, data = .) %>%
    broom::augment()

birthrate_model <-
    birthrate %>%
    filter(date >= ymd(20070101)) %>%
    lm(birthrate ~ date, data = .) %>%
    broom::augment()

avg_flat_rate <- mean(birthrate_model_flat$.fitted)
last_rate <- last(birthrate_model$birthrate)

rate_change <- scales::percent(abs(last_rate - avg_flat_rate) / avg_flat_rate)

geom_downarrow <-
    geom_segment(
        x = ymd(20000101), xend = last(birthrate_model$date),
        y = last_rate, yend = last_rate,
        linetype = "dashed", color = "gray90"
    )

geom_downarrow2 <-
    geom_segment(
        x = ymd(20050101), xend = ymd(20050101),
        y = avg_flat_rate, yend = last_rate,
        arrow = arrow(length = unit(.15, "cm")), color = "gray90"
    )

note <-
    annotate("text",
        x = ymd(20050301), y = mean(c(last_rate, avg_flat_rate)),
        label = rate_change, hjust = 0
    )


birthrate_g <-
    birthrate %>%
    ggplot(aes(x = date, y = birthrate)) +
    geom_point(shape = 1) +
    scale_y_continuous(breaks = seq(0, 25, 2), limits = c(0, NA)) +
    coord_cartesian(ylim = c(0, 19)) +
    geom_line(
        data = birthrate_model,
        aes(x = date, y = .fitted), color = "gray50",
        alpha = .2, linewidth = 2
    ) +
    geom_line(
        data = birthrate_model_flat,
        aes(x = date, y = .fitted), color = "gray50",
        alpha = .2, linewidth = 2
    ) +
    geom_line(
        data = birthrate_model_old,
        aes(x = date, y = .fitted), color = "gray50",
        alpha = .2, linewidth = 2
    ) +
    geom_polygon(
        inherit.aes = FALSE, data = recession_2008,
        aes(x, y),
        alpha = .1,
        fill = "gray10"
    ) +
    geom_downarrow +
    geom_downarrow2 +
    note +
    annotate("text",
        y = 1, x = mean(recession_2008$x),
        label = "2008 Great Recession", angle = 90, hjust = 0
    ) +
    labs(
        x = "", y = "Birth rate (per 1000 people)",
        title = glue::glue(
            "US birth rate has been declining {rate_change}",
            " since the Great Recession"
        )
    )

ggsave("graphs/us_birthrate.png",
    width = 6, height = 4,
    plot = birthrate_g
)
