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

last_date <- format(last(dat)$date, format = "%b %e, %Y")

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
        caption = glue::glue("Source: FRED, GASREGW, GASDESW. Most recent data from week of {last_date}\nShift since first day of hostilities: {startdaycomment} ")
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(.5, .5),
        legend.background = element_rect(color = "gray10", linewidth = .1),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "gray90")
    )

ggsave("dev/gasincrease_since_hostilities.png", width = 10, height = 8, plot = all_g)



av_dat <- dat %>%
    mutate(
        year = year(date),
        x = 1
    ) %>%
    mutate(
        av_price_gas = cumsum(GASREGW) / cumsum(x),
        as_price_des = cumsum(GASDESW) / cumsum(x),
        .by = year
    ) %>%
    filter(year > 2020)

last_by_year <- slice_max(av_dat, date, by = year, n = 1)

av_dat %>%
    ggplot(aes(x = date, y = av_price_gas, group = year)) +
    geom_line() +
    geom_point(aes(y = GASREGW), size = .1, alpha = .2) +
    geom_text(data = last_by_year, aes(label = scales::dollar(av_price_gas)), vjust = -1) +
    geom_point(data = last_by_year) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 0.01)) +
    labs(
        x = NULL,
        y = NULL
    )


pres <- read_csv("sources/presidents.csv") %>%
    select(date = inaugdate, president) %>%
    mutate(president = case_when(date == ymd("2025-01-20") ~ "Donald Trump 2", TRUE ~ president)) %>%
    mutate(CPILFESL = NA, GASREGW = NA, GASDESW = NA) %>%
    relocate(date, CPILFESL, GASREGW, GASDESW, president)


av_dat <- dat %>%
    mutate(president = NA) %>%
    bind_rows(pres) %>%
    arrange(date) %>%
    fill(president, .direction = "down") %>%
    drop_na() %>%
    mutate(
        year = year(date),
        x = 1
    ) %>%
    mutate(
        av_price_gas = cumsum(GASREGW) / cumsum(x),
        av_price_des = cumsum(GASDESW) / cumsum(x),
        .by = president
    )

min_year <- 2010

last_by_year <-
    slice_max(av_dat, date, by = president, n = 1) %>%
    filter(year >= min_year)

max_by_prez <-
    slice_max(av_dat, av_price_des, by = president, n = 1) %>%
    filter(year >= min_year)


des_g <-
    av_dat %>%
    filter(year >= min_year) %>%
    ggplot(aes(x = date, y = av_price_des, group = president)) +
    geom_line() +
    geom_point(aes(y = GASDESW), size = .1, alpha = .2) +
    geom_text(data = last_by_year, aes(label = scales::dollar(av_price_des)), hjust = 0.5, vjust = -1) +
    geom_point(data = last_by_year) +
    geom_point(data = max_by_prez, color = "red") +
    geom_text(data = max_by_prez, color = "red", aes(label = scales::dollar(av_price_des)), hjust = 0.5, vjust = -1) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 0.01)) +
    labs(
        x = NULL,
        y = "Diesel price (in $/gallon)"
    )

gas_g <-
    av_dat %>%
    filter(year >= min_year) %>%
    ggplot(aes(x = date, y = av_price_gas, group = president)) +
    geom_line() +
    geom_point(aes(y = GASREGW), size = .1, alpha = .2) +
    geom_text(data = last_by_year, aes(label = scales::dollar(av_price_gas)), hjust = 0.5, vjust = -1) +
    geom_point(data = last_by_year) +
    geom_point(data = max_by_prez, color = "red") +
    geom_text(data = max_by_prez, color = "red", aes(label = scales::dollar(av_price_gas)), hjust = 0.5, vjust = -1) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 0.01)) +
    labs(
        x = NULL,
        y = "Gas price (in $/gallon)"
    )

ggsave("dev/av_diesel_gas_price.png",
    width = 8, height = 7,
    plot = gas_g / des_g +
        plot_annotation(
            title = "Cumulative average gas and diesel prices by presidential terms",
            caption = "Source: FRED GASREGW and GASDESW. Cumulative averages (in <span style='color:black'><B>black</B></span>) and maxima (in <span style='color:red'>red</span>) by presidential term.",
            theme = theme(
                plot.caption = ggtext::element_textbox_simple()
            )
        )
)
