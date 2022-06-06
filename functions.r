options("getSymbols.warning4.0"=FALSE)


get_index <- function(index = "GDP", src = "FRED") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    tibble::tibble(date = zoo::index(t),
           index = index,
           value = as.numeric(t))
}

retrieve_data <- function(indexes = c("GDP", "GDPC1"), src = "FRED") {
    purrr::map_df(indexes, ~get_index(.x, src))
}

presidentinfo <-
    read_csv("sources/presidents.csv") %>%
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
