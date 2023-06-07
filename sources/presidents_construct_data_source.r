# presidents age in office

library(tidyverse)
library(lubridate)
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"

data_raw <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names() %>%
    select(name_birth_death, term = term_14, party = party_b_15_2)

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
        name_birth_death:party,
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
    ) %>%
    group_by(name) %>%
    mutate(youngest_age_in_office = min(age_in_office)) %>%
    ungroup()

presidents %>%
    filter(year(term_start) >= 1950) %>%
    select(president = name, inaugdate =  term_start, party) %>%
    mutate(party = recode(party, "Republican"  = "R", "Democratic" = "D")) %>%
    write_csv("sources/presidents.csv")