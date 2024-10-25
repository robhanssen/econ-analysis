library(tidyverse)
library(patchwork)
theme_set(theme_light() +
    theme(
        panel.grid.minor.x = element_blank()
    ))

source("functions.r")

dd <- readRDS("dev/adults_in_US.Rds")

pop <- retrieve_data(c("POPTOTUSA647NWDB", "FHHWMC"), "FRED") %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    arrange(date) %>%
    fill(POPTOTUSA647NWDB, .direction = "downup") %>%
    inner_join(dd, by = join_by(date)) %>%
    filter(date >= "1960-01-01") %>%
    janitor::clean_names() %>%
    mutate(ratio = 2 * fhhwmc / adults * 1000)

pop %>%
    ggplot(aes(x = date, y = ratio)) +
    geom_line() + 
    labs(x = "", y = "ratio") + 
    scale_y_continuous(
        limits = c(.3, .7),
        labels = scales::percent_format()
    )


# extraced from WDI csv
# d <- read.csv("clipboard", sep = "\t", header = TRUE)

# dd <- as_tibble(d) %>%
#     pivot_longer(starts_with("X"), names_to = "cat", values_to = "count") %>%
#     reframe(adults = sum(count, na.rm = TRUE), .by = cat) %>%
#     mutate(date = ymd(paste0(as.numeric(str_remove(cat, "X")), "-01-01"))) %>%
#     select(date, adults) %>%
#     filter(adults > 0)
# saveRDS(dd, file = "dev/adults_in_US.Rds")
