library(purrr)

map(list.files(path = "./scripts/", pattern = "*.r", full.names = TRUE),
    source)