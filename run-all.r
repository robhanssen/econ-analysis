library(purrr)

safe_source <- safely(source, quiet = FALSE)

walk(
    list.files(path = "./scripts/", pattern = "*.r$", full.names = TRUE),
    safe_source
)

source("genweb.r")
