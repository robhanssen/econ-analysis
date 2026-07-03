options("getSymbols.warning4.0" = FALSE)


# get_index <- function(index = "GDP", src = "FRED") {
#     t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
#     tibble::tibble(
#         date = zoo::index(t),
#         index = index,
#         value = as.numeric(t)
#     )
# }
source(".api_key.r")
BASE_API_URL <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=##IDX##&api_key=", fred_api_key, "&file_type=json")

get_index <- function(index = "GDP", src = "FRED") {
    if (src == "FRED") {
        url <- gsub("##IDX##", index, BASE_API_URL)
        data <- jsonlite::fromJSON(url)$observations
        data <- data %>%
            mutate(
                index = index,
                date = ymd(date),
                value = as.numeric(value),
                .keep = "none"
            )
    }
    as_tibble(data)
}




retrieve_data <- function(indexes = c("GDP", "GDPC1"), src = "FRED") {
    purrr::map_df(indexes, ~ get_index(.x, src))
}

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
