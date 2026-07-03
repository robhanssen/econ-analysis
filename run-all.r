# library(purrr)
# library(future)
# library(furrr)

future::plan(future::multisession, workers = 12)

safe_source <- purrr::safely(source, quiet = FALSE)

furrr::future_walk(
    list.files(path = "./scripts/", pattern = "*.r$", full.names = TRUE),
    safe_source
)

source("genweb.r")