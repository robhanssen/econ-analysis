# presidents age in office

library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    select(name_birth_death, term = term_14)

data_cleaned <-
    data_raw %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[\\d{2}\\]")
    )) %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[\\d{1}\\]")
    )) %>%
    mutate(across(
        name_birth_death:term,
        ~ str_remove_all(.x, "\\[[[:lower:]]\\]")
    )) %>%
    separate(name_birth_death, into = c("name", "birth_death"), sep = "\\(") %>%
    mutate(across(birth_death, ~ str_remove_all(.x, "(\\)|b\\.)"))) %>%
    separate(birth_death, into = c("birth", "death"), sep = "\\–") %>%
    separate(term, into = c("term_start", "term_end"), sep = "\\–") %>%
    mutate(across(starts_with("term"), ~ as.Date(.x, format = "%B %d, %Y"))) %>%
    mutate(across(
        birth:death,
        ~ lubridate::floor_date(as.Date(.x, format = "%Y"), unit = "year")
    ))

presidents <-
    data_cleaned %>%
    mutate(
        age = (death - birth) / lubridate::dyears(1),
        alive = is.na(age),
        age_in_office = (term_start - birth) / lubridate::dyears(1),
        age_out_office = (term_end - birth) / lubridate::dyears(1),
        end_bar = ifelse(is.na(term_end),
            (lubridate::today() - birth) / dyears(1),
            age_out_office
        ),
        current_age = ifelse(!is.na(age), NA,
            (lubridate::today() - birth) / dyears(1)
        )
    )

presidents %>%
    mutate(name = fct_reorder(name, age_in_office)) %>%
    ggplot() +
    aes(y = name) +
    geom_point(aes(x = age_in_office, color = alive), size = 3) +
    geom_point(aes(x = age_out_office, color = alive), size = 3) +
    geom_point(aes(x = current_age, color = alive), shape = 5, size = 3) +
    geom_segment(aes(
        yend = name,
        x = age_in_office,
        xend = end_bar,
        color = alive
    ),
    size = 3
    ) +
    geom_vline(
        xintercept = mean(presidents$age_in_office),
        lty = 3
    ) +
    geom_vline(
        xintercept = mean(presidents$age_out_office, na.rm = TRUE),
        lty = 3
    ) +
    scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70")) +
    labs(x = "Age", y = "") +
    theme(legend.position = "none")

ggsave("elections/president_ages.png", width = 8, height = 6)