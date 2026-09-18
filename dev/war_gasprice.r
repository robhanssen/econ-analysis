library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)
        )
)

source("functions.r")

dat <-
    retrieve_data(c("CPILFESL", "GASREGW")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    # filter(DCOILWTICO > 0) %>%
    arrange(date) %>%
    mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    fill(c("GASREGW", "CPILFESL"), .direction = "downup") %>%
    filter(date > "1992-01-01")


war_start <- c("Ukraine" = ymd(20220223), "Iran" = ymd(20260227))

iran_war_length <- max(ymd(20261231) - war_start["Iran"], max(dat$date) - war_start["Iran"])


war_gas <-
    bind_rows(
        dat %>%
            filter(date >= war_start["Ukraine"], date <= war_start["Ukraine"] + iran_war_length) %>%
            mutate(
                war = "Ukraine",
                day = as.numeric(date - war_start["Ukraine"])
            ),
        dat %>%
            filter(date >= war_start["Iran"]) %>%
            mutate(
                war = "Iran",
                day = as.numeric(date - war_start["Iran"])
            )
    )


color <- c("Iran" = "black", "Ukraine" = "red")
startdaycomment <- paste0(names(war_start), ": ", format(war_start, format = "%b %e, %Y"), collapse = " | ")

abs_g <- war_gas %>%
    mutate(gas_diff = GASREGW - first(GASREGW), .by = war) %>%
    ggplot(aes(x = day, y = gas_diff, color = war)) +
    geom_line(linewidth = .75, show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_dollar()
    ) +
    scale_color_manual(values = color) +
    annotate(geom = "text", x = c(200, 200), y = c(1.10, 0.30), label = c("Iran", "Ukraine"), color = color) +
labs(
    x = "Days since war outbreak",
    y = "Gasoline price shift ($/gallon)",
    # caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
)




rel_g <- 
    war_gas %>%
    mutate(gas_diff = GASREGW / first(GASREGW) - 1, .by = war) %>%
    ggplot(aes(x = day, y = gas_diff, color = war)) +
    geom_line(linewidth = .75, show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    scale_color_manual(values = color) +
    annotate(geom = "text", x = c(180, 200), y = c(0.32, 0.10), label = c("Iran", "Ukraine"), color = color) +
labs(
    x = "Days since war outbreak",
    y = "Relative gasoline price shift (%)",
    # caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
)


# all_g <- 
#     abs_g + rel_g + 
#     plot_annotation(
#         title = "Absolute and relative gasoline price shift comparison since beginning of hostilities",
#         caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
#     )


# ggsave("dev/gasincrease_since_hostilities.png", width = 10, height = 5, plot = all_g)



desdat <-
    retrieve_data(c("CPILFESL", "GASDESW")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    # filter(DCOILWTICO > 0) %>%
    arrange(date) %>%
    mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    fill(c("GASDESW", "CPILFESL"), .direction = "downup") %>%
    filter(date > "1992-01-01")


war_des <-
    bind_rows(
        desdat %>%
            filter(date >= war_start["Ukraine"], date <= war_start["Ukraine"] + iran_war_length) %>%
            mutate(
                war = "Ukraine",
                day = as.numeric(date - war_start["Ukraine"])
            ),
        desdat %>%
            filter(date >= war_start["Iran"]) %>%
            mutate(
                war = "Iran",
                day = as.numeric(date - war_start["Iran"])
            )
    )


des_abs_g <- war_des %>%
    mutate(gas_diff = GASDESW - first(GASDESW), .by = war) %>%
    ggplot(aes(x = day, y = gas_diff, color = war)) +
    geom_line(linewidth = .75, show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_dollar()
    ) +
    scale_color_manual(values = color) +
    annotate(geom = "text", x = c(200, 200), y = c(1.50, 0.50), label = c("Iran", "Ukraine"), color = color) +
labs(
    x = "Days since war outbreak",
    y = "Diesel price shift ($/gallon)",
    # caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
)




des_rel_g <- 
    war_des %>%
    mutate(gas_diff = GASDESW / first(GASDESW) - 1, .by = war) %>%
    ggplot(aes(x = day, y = gas_diff, color = war)) +
    geom_line(linewidth = .75, show.legend = FALSE) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    scale_color_manual(values = color) +
    annotate(geom = "text", x = c(180, 200), y = c(0.33, 0.15), label = c("Iran", "Ukraine"), color = color) +
labs(
    x = "Days since war outbreak",
    y = "Relative diesel price shift (%)",
    # caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
)


all_des_g <- 
    des_abs_g + des_rel_g + 
    plot_annotation(
        title = "Absolute and relative diesel price shift comparison since beginning of hostilities",
        caption = glue::glue("Source: FRED, GASREGW. Shift since first day of hostilities {startdaycomment} ")
    )

all_all_g <- 
    (abs_g + rel_g )/(des_abs_g + des_rel_g ) + 
    plot_annotation(
        title = "Absolute and relative gas and diesel price shift comparison since beginning of hostilities",
        caption = glue::glue("Source: FRED, GASREGW, GASDESW. Shift since first day of hostilities: {startdaycomment} ")
    )



ggsave("dev/gasincrease_since_hostilities.png", width = 14, height = 10, plot = all_all_g)
