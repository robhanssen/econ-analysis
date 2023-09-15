images <- list.files(path = "graphs/", pattern = ".*\\.png$", full.names = TRUE)

links <- paste0(
    "<p><img src=\"",
    images,
    "\" width = \"50%\" alt =\"",
    stringr::str_remove_all(images, "(^.*/|\\.png$)"),
    "\"></p><hr/>", collapse = ""
)


header <-
    paste0(
        "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",
        "<html><title>Econ Analysis</title><body>"
    )

footer <-
    paste0(
        "</body></html>"
    )

fullpage <-
    paste0(header, links, footer, collapse = "")


writeLines(fullpage, con = "econ-analysis.html")
