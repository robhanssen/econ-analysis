images <- list.files(path = "graphs/", pattern = ".*\\.png$", full.names = TRUE)
links <- paste0("<p><img src=\"", images, "\" width = \"50%\"></p><hr>")


header <-
    paste0(
        "<html><title>Econ Analysis</title>
        <body>"
    )

footer <-
    paste0(
        "</body></html>"
    )

fullpage <-
    paste0(header, paste0(links, collapse = ""), footer, collapse = "")


writeLines(fullpage, con = "econ-analysis.html")
