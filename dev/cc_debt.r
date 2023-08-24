library(tidyverse)

source("functions.r")

raw_data <-
    retrieve_data(
        c(
            "CCLACBW027SBOG",
            "CPIAUCSL",
            "POPTOTUSA647NWDB",
            "GDP"
        )
    )

ccdebt_processed <-
    raw_data %>%
    pivot_wider(names_from = index, values_from = value) %>%
    rename(
        ccdebt = CCLACBW027SBOG,
        cpi = CPIAUCSL,
        pop = POPTOTUSA647NWDB,
        gdp = GDP
    ) %>%
    arrange(date) %>%
    mutate(across(
        !date,
        ~ zoo::na.approx(.x, na.rm = FALSE)
    )) %>%
    fill(c("cpi", "pop", "gdp"), .direction = "downup") %>%
    filter(date > ymd(20120101)) %>%
    mutate(
        ccdebt_cpi = ccdebt * first(cpi) / cpi,
        ccdebt_cpi_pop = ccdebt_cpi * first(pop) / pop,
        ccdebt_pop = ccdebt * first(pop) / pop,
    )


long_format_data <-
    ccdebt_processed %>%
    pivot_longer(starts_with("ccdebt"),
        names_to = "index",
        values_to = "value")

labels <- 
    long_format_data %>%
    slice_max(date, by = index) %>%
    select(date, value, index)

long_format_data %>%
    ggplot(aes(x = date, y = value, color = index)) +
    geom_line() +
    scale_y_continuous(limits = c(400, NA)) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .5) +
    ggrepel::geom_label_repel(
        data = labels,
        hjust = 1,
        aes(x = date, y = value, label = index)
        ) + 
    theme(legend.position = "none")