library(tidyverse)
library(patchwork)
library(lubridate)

theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.grid.minor = element_blank()
    ))

source("functions.r")

income <-
    retrieve_data(indexes = c("W875RX1", "LFWA64TTUSM647S"), "FRED") %>%
    pivot_wider(names_from = "index") %>%
    janitor::clean_names() %>%
    mutate(real_income = w875rx1 / lfwa64ttusm647s * 1e9) %>%
    arrange(date) %>%
    filter(date > "1972-12-31")


trend1 <-
    income %>%
    filter(date > "2003-03-01", date < "2007-06-02") %>%
    lm(real_income ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(20030301), today(), by = "month"))) %>%
    rename(real_income = .fitted)

trend2 <-
    income %>%
    filter(date > "2011-06-01", date < "2019-06-02") %>%
    lm(real_income ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(20110601), today(), by = "month"))) %>%
    rename(real_income = .fitted)

trend_exp <-
    income %>%
    filter(date > "1974-03-01", date < "2007-06-02") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(19740301), today(), by = "month"))) %>%
    mutate(real_income = exp(.fitted))

trend_exp2 <-
    income %>%
    filter(date > "2010-03-01", date < "2019-06-02") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(20100301), today(), by = "month"))) %>%
    mutate(real_income = exp(.fitted))

trend_exp3 <-
    income %>%
    filter(date > "2022-03-01") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(20220301), today(), by = "month"))) %>%
    mutate(real_income = exp(.fitted))


x <- income %>%
    filter(date > "2022-03-01") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::tidy() %>%
    pull("estimate")
exp(x * 365)[2]

x <- income %>%
    filter(date > "1974-03-01", date < "2007-06-02") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::tidy() %>%
    pull("estimate")
exp(x * 365)[2]


x <- income %>%
    filter(date > "2010-03-01", date < "2019-06-02") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::tidy() %>%
    pull("estimate")
exp(x * 365)


exp_income <- bind_rows(
    last(trend_exp),
    last(trend_exp2),
    last(trend_exp3)
)


income %>%
    ggplot(
        aes(x = date, y = real_income)
    ) +
    geom_point(shape = 1, alpha = .2) +
    geom_line(data = trend_exp, color = "red") +
    geom_line(data = trend_exp2, color = "red") +
    geom_line(data = trend_exp3, color = "red") +
    scale_y_continuous(
        labels = scales::label_dollar(accuracy = 1, scale = 1, suffix = "")
    ) +
    labs(
        x = "",
        y = "Real personal income (in USD)",
        title = "Real income (without government transfers) per working-age person in the US",
        caption = "Income in chained 2017 dollars\nSource: FRED W875RX1 and LFWA64TTUSM647S"
    ) +
    ggrepel::geom_label_repel(
        data = exp_income,
        aes(x = date, y = real_income, label = round(real_income, digits = 0)),
        hjust = 1
    )

ggsave("dev/real_income.png", width = 8, height = 5)