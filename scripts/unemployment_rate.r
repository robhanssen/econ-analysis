library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("functions.r")

unrate_raw <- retrieve_data("UNRATE", "FRED")

unrate <- unrate_raw

nlsmods <- function(tbl) {
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = tbl
    )
}

# define periods to investigate
periods <- tribble(
    ~name, ~pstart, ~pend,
    "1957-1959", ymd(19570701), ymd(19590501),
    "1970-1973", ymd(19700101), ymd(19730101),
    "1974-1979", ymd(19740101), ymd(19790101),
    "1981-1984", ymd(19810101), ymd(19840601),
    "1991-1998", ymd(19900101), ymd(19980101),
    "2000-2005", ymd(20000101), ymd(20070101),
    "2010-2020", ymd(20080101), ymd(20200101)
) %>%
    mutate(period = pstart %--% pend)

unrate$name <- NA

for (i in seq_along(unrate$date)) {
    for (j in seq_along(periods$name)) {
        if (unrate$date[i] %within% periods$period[j]) {
            unrate$name[i] <- periods$name[j]
        }
    }
}

modelcollection <-
    unrate %>%
    filter(!is.na(name)) %>%
    group_by(name) %>%
    mutate(time = (date - min(date) + 1) / dmonths(1)) %>%
    nest() %>%
    mutate(mod = purrr::map(data, ~ nlsmods(.)))

fittedmodels <-
    modelcollection %>%
    mutate(fitteddata = map(mod, broom::augment)) %>%
    left_join(periods, by = "name") %>%
    unnest(fitteddata) %>%
    mutate(date = as.Date(pstart + time * dmonths(1))) %>%
    ggplot() +
    aes(date, .fitted, color = name) +
    geom_line(size = 1) +
    geom_point(data = unrate, aes(x = date, y = value), alpha = .3) +
    scale_y_continuous(
        breaks = seq(0, 20, 2),
        labels = scales::number_format(
            accuracy = .1,
            suffix = "%"
        ),
        limits = c(0, 14)
    ) +
    labs(
        x = "Date",
        y = "Employment (in %)"
    ) +
    theme(legend.position = "none")

limitvalues <-
    modelcollection %>%
    mutate(params = map(mod, broom::tidy)) %>%
    unnest(params) %>%
    filter(term == "a0") %>%
    mutate(errbar = std.error * qnorm(0.975)) %>%
    ggplot() +
    aes(name, estimate, fill = name) +
    geom_col() +
    geom_errorbar(aes(
        ymin = estimate - errbar,
        ymax = estimate + errbar
    ),
    width = .2
    ) +
    scale_y_continuous(
        breaks = seq(0, 20, 2),
        labels = scales::number_format(
            accuracy = .1,
            suffix = "%"
        ),
        limits = c(0, 14)
    ) +
    labs(
        x = "Period",
        y = "Limit unemployment (in %)"
    ) +
    theme(legend.position = "none")

peakvalues <-
    modelcollection %>%
    mutate(params = map(mod, broom::tidy)) %>%
    unnest(params) %>%
    filter(term %in% c("a0", "a1")) %>%
    select(name, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(peak = a0 + a1) %>%
    ggplot() +
    aes(name, peak, fill = name) +
    geom_col() +
    scale_y_continuous(
        breaks = seq(0, 20, 2),
        labels = scales::number_format(
            accuracy = .1,
            suffix = "%"
        ),
        limits = c(0, 14)
    ) +
    labs(
        x = "Period",
        y = "Peak unemployment (in %)"
    ) +
    theme(legend.position = "none")


p <- fittedmodels / (peakvalues + limitvalues)

ggsave("graphs/unemployment-analysis.png", width = 15, height = 12, plot = p)