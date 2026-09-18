library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot"
        )
)

source("functions.r")

color <- c("Iran" = "black", "Ukraine" = "red")
war_start <- c("Ukraine" = ymd(20220223), "Iran" = ymd(20260227))
startdaycomment <- paste0(names(war_start), ": ", format(war_start, format = "%b %e, %Y"), collapse = " | ")

select_war <- function(dat, war) {
    filter(dat, date >= war_start[war], date <= war_start[war] + iran_war_length) %>%
        mutate(
            war = war,
            day = as.numeric(date - war_start[war])
        )
}

dat <-
    retrieve_data(c("CPILFESL", "GASREGW", "GASDESW")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    arrange(date) %>%
    mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    fill(c("GASREGW", "CPILFESL", "GASDESW"), .direction = "downup")

iran_war_length <- max(ceiling_date(ymd(today()), "year") - war_start["Iran"], max(dat$date) - war_start["Iran"])

war_gas <-
    map_dfr(names(war_start), ~ select_war(dat, .x)) %>%
    mutate(
        gas_diff_abs = GASREGW - first(GASREGW),
        gas_diff_rel = GASREGW / first(GASREGW) - 1,
        des_diff_abs = GASDESW - first(GASDESW),
        des_diff_rel = GASDESW / first(GASDESW) - 1,
        .by = war
    ) %>%
    select(war, day, ends_with("rel"), ends_with("abs")) %>%
    pivot_longer(-c(day, war)) %>%
    mutate(
        fueltype = ifelse(str_detect(name, "gas"), "Gas", "Diesel"),
        datatype = ifelse(str_detect(name, "abs"), "Absolute", "Relative"),
    )

all_g <-
    war_gas %>%
    ggplot(aes(x = day, y = value, color = war)) +
    geom_line(show.legend = TRUE) +
    ggh4x::facet_grid2(datatype ~ fueltype, scales = "free_y") +
    ggh4x::facetted_pos_scales(
        y = list(
            datatype == "Relative" ~ scale_y_continuous(labels = scales::label_percent()),
            datatype == "Absolute" ~ scale_y_continuous(labels = scales::label_dollar(accuracy = 0.01))
        )
    ) +
    scale_color_manual(values = color) +
    labs(
        x = "Days since war outbreak",
        y = "Price shift (% or $)",
        color = "Conflict",
        title = "Absolute and relative gas and diesel price shift comparison since beginning of hostilities",
        caption = glue::glue("Source: FRED, GASREGW, GASDESW. Shift since first day of hostilities: {startdaycomment} ")
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(.5, .5),
        legend.background = element_rect(color = "gray10", linewidth = .1),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "gray90")
    )

ggsave("dev/gasincrease_since_hostilities.png", width = 10, height = 8, plot = all_g)
