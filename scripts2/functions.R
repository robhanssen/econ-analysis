options("getSymbols.warning4.0" = FALSE)


get_index <- function(index, src = "FRED") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    tibble::tibble(
        date = zoo::index(t),
        index = index,
        value = as.numeric(t)
    )
}

retrieve_data <- function(indexes, src = "FRED") {
    purrr::map_df(indexes, ~ get_index(.x, src))
}


pivot_and_interpolate <- function(dat, interpolation = TRUE, col_names = NA_character_) {
    d <- pivot_wider(dat, names_from = "index", values_from = "value")
    if (all(is.na(col_names))) {
        return(d)
    } else {
        add_names <- c("date", col_names)
        d <- d %>% set_names(add_names)
    }

    if(!interpolation) return(d) 
    else d <- d %>% mutate(across(!date, interpolate_data))

    return(d)
}

interpolate_data <- function(dat) {
    zoo::na.approx(dat, na.rm = FALSE)
}


retrieve_data(c("GDP", "PAYEMS")) %>% pivot_and_interpolate(interpolation = TRUE, col_names = c("gdp", "jobs"))

xxx <- 1:30
xxx[10:20] <- NA
xxx[28:30] <- NA
xxx[1:3] <- NA

interpolate(xxx)

presidentinfo <-
    read_csv("sources/presidents.csv", col_types = "cDc") %>%
    mutate(
        party = factor(party, levels = c("R", "D"))
    ) %>%
    mutate(floordate = ceiling_date(inaugdate, "quarter"))

inaugdates <- c(
    ymd("1776-03-31"),
    presidentinfo$inaugdate,
    ymd("9999-12-31")
)

presidents <- c("", presidentinfo$president)
partycolor <- c("R" = "red", "D" = "blue")
