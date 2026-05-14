library(tidyverse)


FRED_BASE_URL <- "https://api.stlouisfed.org/fred/series/observations?series_id=__INDEX__&api_key=__APIKEY__&file_type=json"
FRED_API_KEY <- fred_api_key

compose_url <- function(index, base_url, apikey) {
    glue::glue(base_url) %>%
        str_replace("__INDEX__", index) %>%
        str_replace("__APIKEY__", apikey)
}

get_index <- function(index, base_url = FRED_BASE_URL, apikey = FRED_API_KEY) {
    url <- compose_url(index, base_url, apikey)
    fred_request <- httr2::request(url)
    resp_fred <- httr2::req_perform(fred_request)

    index_data <- httr2::resp_body_json(resp_fred) %>%
        pluck("observations") %>%
        map_dfr(as_tibble) %>%
        mutate(index = index) %>%
        select(date, index, value) %>%
        mutate(
            date = ymd(date),
            value = as.numeric(value)
        )

    index_data
}


retrieve_data <- function(indexes = c("GDP", "GDPC1"), base_url = FRED_BASE_URL, apikey = FRED_API_KEY) {
    purrr::map_dfr(indexes, ~ get_index(.x, base_url, apikey)) %>%
        arrange(date)
}
