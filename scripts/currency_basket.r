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

    strata <- universalcoin %>%
        filter(date >= lubridate::ymd(20000101)) %>%
        summarize(x = first(totalcoin)) %>%
        pull(x)

    universalcoin %>%
        ggplot() +
        aes(date, totalcoin / strata) +
        geom_line() +
        geom_hline(yintercept = 1, alpha = .5) +
        labs(
            x = "Date",
            y = paste(str)
        )
}

p <- currency_graph(euro) /
    currency_graph(pound) /
    currency_graph(usd) /
    currency_graph(cad) /
    currency_graph(yuan) /
    currency_graph(yen) /
    currency_graph(won) /
    currency_graph(rupee) +
    plot_annotation(
        title = "Relative strength of currency vs a basket of currencies",
        subtitle = "Indexed to Jan 1, 2000"
    )

ggsave("graphs/currency-basket.png", plot = p, height = 16, width = 8)