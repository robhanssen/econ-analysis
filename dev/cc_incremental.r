library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            panel.grid.minor.y = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

source("functions.r")

raw_data <-
    retrieve_data(
        c(
            "REVOLSL",
            "CPIAUCSL",
            "POP",
            "GDP"
        )
    )

cpi_year <- "2012"

ccdebt_processed <-
    raw_data %>%
    pivot_wider(names_from = index, values_from = value) %>%
    rename(
        ccdebt = REVOLSL,
        cpi = CPIAUCSL,
        pop = POP,
        gdp = GDP
    ) %>%
    arrange(date) %>%
    mutate(across(
        !date,
        ~ zoo::na.approx(.x, na.rm = FALSE)
    )) %>%
    fill(c("cpi", "pop", "gdp"), .direction = "downup") %>%
    filter(date >= ymd(glue::glue("{cpi_year}-01-01"))) %>%
    mutate(
        cc_inc = ccdebt - lag(ccdebt),
        ccdebt_cpi = ccdebt * first(cpi) / cpi,
        ccdebt_cpi_pop = ccdebt_cpi * first(pop) / pop,
        cc_inc_cpi_pop = ccdebt_cpi_pop - lag(ccdebt_cpi_pop),
        ccdebt_pop = ccdebt * first(pop) / pop,
    ) %>%
    drop_na() # remove NAs caused by POP predictions

ccdebt_processed %>%
    arrange(date) %>%
    filter(date >= "2021-01-01") %>%
    mutate(cc_inc_sweep = zoo::rollmeanr(cc_inc, 3,  na.pad = TRUE)) %>% view()
    mutate(  positive = cc_inc  > 0) %>%
    ggplot(aes(date, y = cc_inc , fill = positive)) + 
    geom_col() + 
    geom_line(aes(y = cc_inc_sweep)) + 
    scale_fill_manual(values = c("TRUE" = "dodgerblue","FALSE" = "red"))


ccdebt_processed %>%
    # filter(date >= "2021-01-01") %>%
    mutate(  positive = cc_inc_cpi_pop  > 0) %>%
    ggplot(aes(date, y =  cc_inc_cpi_pop, fill = positive)) + 
    geom_col() + 
    scale_fill_manual(values = c("TRUE" = "dodgerblue","FALSE" = "red"))



ccdebt_processed %>%
    mutate(year = year(date),
      ) %>%
    filter(year > 2012) %>%
    reframe(
        sum(cc_inc_cpi_pop),
        .by = year
    ) 


ccdebt_processed %>%
    filter(date > "2021-01-01") %>%
    mutate(  positive = cc_inc_cpi_pop  > 0) %>%
    ggplot(aes(date, y = cc_inc_cpi_pop , color = positive, group = 1)) + 
    # geom_col() + 
    geom_point() + geom_line() +
    scale_fill_manual(values = c("TRUE" = "dodgerblue","FALSE" = "red"))

