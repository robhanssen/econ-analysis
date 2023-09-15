library(purrr)

walk(list.files(path = "./scripts/", pattern = "*.r", full.names = TRUE),
    source)

source("genweb.r")