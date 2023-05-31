# senators age in office

library(tidyverse)
library(lubridate)
library(rvest)

theme_set(theme_light() +
    theme(
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
    ))

url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_senators"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    select(state, senator, party_2, born, assumed_office)

data_cleaned <-
    data_raw %>%
    mutate(across(
        everything(),
        ~ str_remove_all(.x, "\\[[[:lower:]]\\]")
    )) %>%
    mutate(born = ymd(str_remove_all(born, "\\).*|\\("))) %>%
    mutate(assumed_office = mdy(assumed_office)) %>%
    mutate(party_2 = str_remove(party_2, "\\(DFL\\)")) %>%
    mutate(party = factor(party_2)) %>%
    select(-party_2)

senators <-
    data_cleaned %>%
    mutate(
        age = (today() - born) / lubridate::dyears(1),
        age_in_office = (assumed_office - born) / lubridate::dyears(1),
        time_in_office = (today() - assumed_office) / lubridate::dyears(1),
        state_abb = state.abb[state == state]
    )

sen_plot <-
    senators %>%
    mutate(name = fct_reorder(senator, age)) %>%
    ggplot() +
    aes(y = name) +
    geom_segment(
        aes(
            yend = name,
            x = age_in_office,
            xend = age,
            color = party
        ),
        linewidth = 3
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        limits = c(30, 95),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    scale_color_manual(
        values =
            c(
                "Democratic" = "blue",
                "Republican" = "red",
                "Independent" = "gray70"
            )
    ) +
    labs(x = "Age", y = "") +
    geom_vline(
        xintercept = 77.27, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 78.27, y = 25,
        hjust = 0, label = "US life\nexpectancy"
    ) +
    geom_vline(
        xintercept = 67, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 68, y = 15,
        hjust = 0, label = "US age of\nretirement"
    ) +
    geom_vline(
        xintercept = 55, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    annotate("text",
        x = 56, y = 5,
        hjust = 0, label = "AARP membership\nminimum age"
    )


ggsave("elections/senator_ages.png",
    width = 6, height = 12,
    plot = sen_plot
)

time_sen_plot <-
    senators %>%
    mutate(name = fct_reorder(senator, time_in_office)) %>%
    ggplot() +
    aes(y = name) +
    geom_col(aes(x = time_in_office, fill = party)) +
    scale_x_continuous(
        breaks = seq(0, 100, 10),
        # limits = c(35, NA),
        sec.axis = sec_axis(~.,
            breaks = seq(0, 100, 10)
        )
    ) +
    scale_fill_manual(
        values =
            c(
                "Democratic" = "blue",
                "Republican" = "red",
                "Independent" = "gray70"
            )
    ) +
    labs(x = "Time in office (in years)", y = "")

ggsave("elections/senator_time_in_office.png",
    width = 6, height = 12,
    plot = time_sen_plot
)