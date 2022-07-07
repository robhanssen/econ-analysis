library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())


source("functions.r")

unrate <- retrieve_data("UNRATE", "FRED") %>%
    filter(date > ymd(19700101))

nlsmods <- function(tbl) {
    nls(value ~ a0 + a1 * exp(-1 / a3 * (log(time) - a2)^2),
        start = list(a0 = 4, a1 = 6, a2 = 3.5, a3 = 1),
        data = tbl
    )
}

# define periods to investigate
periods <- tribble(
    ~name, ~pstart, ~pend,
    "1948-1951", ymd(19490101), ymd(19510101),
    "1970-1973", ymd(19700101), ymd(19730101),
    "1974-1979", ymd(19740101), ymd(19790101),
    "1981-1984", ymd(19810101), ymd(19840101),
    "1990-1995", ymd(19900101), ymd(19950101),
    "2000-2005", ymd(20000101), ymd(20050101),
    "2010-2020", ymd(20080101), ymd(20200101)
) %>%
    mutate(period = pstart %--% pend)

unrate$name <- NA

for (i in 1:nrow(unrate)) {
    for (j in 1:nrow(periods)) {
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
        labels = scales::number_format(
            accuracy = .1,
            suffix = "%"
        ),
        limits = c(0, 14)
    ) +
    theme(legend.position = "none") +
    modelcollection %>%
    mutate(params = map(mod, broom::tidy)) %>%
    unnest(params) %>%
    filter(term == "a0") %>%
    ggplot() +
    aes(name, estimate, fill = name) +
    geom_col() +
    scale_y_continuous(
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