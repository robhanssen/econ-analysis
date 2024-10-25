library(tidyverse)
library(patchwork)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

source("functions.r")

# api <- "https://api.eia.gov/v2/coal/consumption-and-quality/data/?frequency=quarterly&data[0]=consumption&data[1]=sulfur-content&sort[0][column]=period&sort[0][direction]=desc&offset=29400&length=5000"


api <-
    glue::glue(
        "https://api.eia.gov/v2/coal/consumption-and-quality/data/?",
        "frequency=quarterly&",
        "data[0]=consumption&data[1]=sulfur-content&sort[0][column]=period&",
        "sort[0][direction]=desc&",
        # "offset=29400&",
        "offset=0&",
        # "length=5000&",
        "api_key={api_key}"
    )

spr_raw <- jsonlite::read_json(api)

spr_cleaned <-
    as_tibble(spr_raw$response) %>%
    unnest_wider(data) %>%
    janitor::clean_names() %>%
    mutate(
        period = yq(period),
        consumption = as.numeric(consumption),
        sector_description = factor(sector_description)
    ) %>%
    select(period, consumption, state_description, location,sector_description) %>%
    arrange(period)


