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
    # "rub", 1.574
) %>%
    mutate(fraction = gdp / sum(gdp))

# rub_raw <- quantmod::getSymbols(
#     "RUB=X",
#     src = "yahoo",
#     auto.assign = FALSE
# )

# usdrub <- tibble(
#     date = zoo::index(rub_raw),
#     rub = as.numeric(rub_raw$"RUB=X.Close")
# ) %>%
#     filter(rub > 10)

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
# %>%
# left_join(usdrub)

currency_graph <- function(curr = usd, datadf = dolexch, wght = curr_vec, from_date = lubridate::ymd(20000101)) {
    coins <- colnames(datadf)[-1]

    str <- toupper(rlang::as_label(rlang::enquo(curr)))

    univexch <-
        datadf %>%
        mutate(across(!date, ~ .x / {{ curr }}))

    universalcoin <-
        map_dfc(coins, ~ tibble(.x, wght[.x] * univexch[, .x])) %>%
        select(all_of(coins)) %>%
        bind_cols(date = datadf$date) %>%
        pivot_longer(-date) %>%
        group_by(date) %>%
        summarize(totalcoin = sum(value), .groups = "drop")

    strata <- universalcoin %>%
        filter(date >= lubridate::ymd(20080103)) %>%
        summarize(x = first(totalcoin)) %>%
        pull(x)

    universalcoin %>%
        filter(date >= from_date) %>%
        ggplot() +
        aes(date, totalcoin / strata) +
        geom_line() +
        geom_hline(yintercept = 1, alpha = .5) +
        labs(
            x = "Date",
            y = paste(str)
        )
}

first_date <- ymd("20000101") # first(dolexch$date)


p <- currency_graph(euro, from_date = first_date) /
    currency_graph(pound, from_date = first_date) /
    currency_graph(usd, from_date = first_date) /
    currency_graph(cad, from_date = first_date) /
    currency_graph(yuan, from_date = first_date) /
    # currency_graph(rub, from_date = first_date) /
    currency_graph(yen, from_date = first_date) /
    currency_graph(won, from_date = first_date) /
    currency_graph(rupee, from_date = first_date) +
    plot_annotation(
        title = "Relative strength of currency vs a basket of currencies",
        subtitle = "Indexed to Jan 1, 2000"
    )

ggsave("graphs/currency-basket.png", plot = p, height = 16, width = 8)
