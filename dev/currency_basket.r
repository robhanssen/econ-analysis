library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("functions.r")

gdp <- tribble(
    ~currency, ~gdp,
    # 2017 worldometer data
    "usd", 19.485,
    "yuan", 12.238,
    "yen", 4.872,
    "euro", 16.6,
    "rupee", 3.693,
    "pound", 2.638,
    "real", 2.054,
    "cad", 1.647,
    "won", 1.531
) %>%
    mutate(fraction = gdp / sum(gdp))

curr_vec <- setNames(gdp$fraction, gdp$currency)

dolexch <-
    retrieve_data(indexes = c(
        "DEXJPUS", # Japanese Yen
        "DEXUSUK", # British Pound
        "DEXUSEU", # Euro
        "DEXCHUS", # Chinese Yuan
        "DEXINUS", # Indian Rupee
        "DEXBZUS", # Brazilean Real
        "DEXCAUS", # Canadian Dollar
        "DEXKOUS" # Korean Won
    )) %>%
    pivot_wider(names_from = "index", values_from = "value") %>%
    mutate(across(!date & !ends_with("US"), ~ 1 / .x)) %>%
    drop_na() %>%
    rename(
        pound = "DEXUSUK",
        euro = "DEXUSEU",
        yen = "DEXJPUS",
        yuan = "DEXCHUS",
        rupee = "DEXINUS",
        real = "DEXBZUS",
        cad = "DEXCAUS",
        won = "DEXKOUS"
    ) %>%
    mutate(usd = 1)


coins <- colnames(dolexch)[-1]

doluniversal <-
    map_dfc(coins, ~ tibble(.x, curr_vec[.x] * dolexch[, .x])) %>%
    select(coins) %>%
    bind_cols(date = dolexch$date) %>%
    pivot_longer(-date) %>%
    group_by(date) %>%
    summarize(totalcoin = sum(value), .groups = "drop")

strata <- mean(doluniversal$totalcoin)

doluniversal %>%
    ggplot() +
    aes(date, totalcoin / strata) +
    geom_line() +
    labs(
        x = "Date",
        y = "Dollar value compared\nto basket of currencies"
    )


currency_graph <- function(curr = usd, datadf = dolexch, wght = curr_vec) {
    coins <- colnames(datadf)[-1]

    str <- toupper(rlang::as_label(rlang::enquo(curr)))

    univexch <-
        datadf %>%
        mutate(across(!date, ~ .x / {{ curr }}))

    universalcoin <-
        map_dfc(coins, ~ tibble(.x, wght[.x] * univexch[, .x])) %>%
        select(coins) %>%
        bind_cols(date = datadf$date) %>%
        pivot_longer(-date) %>%
        group_by(date) %>%
        summarize(totalcoin = sum(value), .groups = "drop")

    strata <- mean(universalcoin$totalcoin)

    universalcoin %>%
        ggplot() +
        aes(date, totalcoin / strata) +
        geom_line() +
        labs(
            x = "Date",
            y = paste(str, "value compared\nto basket of currencies")
        )
}

p <- currency_graph(euro) / currency_graph(pound) / currency_graph(usd)

ggsave("dev/basket.png", plot = p)