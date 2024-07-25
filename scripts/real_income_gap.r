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

trend_exp <-
    income %>%
    filter(date > "1974-03-01", date < "2007-06-02") %>%
    lm(log(real_income) ~ date, data = .) %>%
    broom::augment(newdata = tibble(date = seq(ymd(19740301), today(), by = "month"))) %>%
    mutate(real_income = exp(.fitted))

realincome_g <- income %>%
    ggplot(
        aes(x = date, y = real_income)
    ) +
    geom_point(shape = 1, alpha = .2) +
    geom_line(data = trend_exp, color = "gray30") +
    scale_y_continuous(
        labels = scales::label_dollar(accuracy = 1, scale = 1, suffix = "")
    ) +
    labs(
        x = "",
        y = "Real personal income (in USD)",
        title = "Real income (without government transfers) per working-age person in the US"
    )

effective <-
    inner_join(income, trend_exp, by = join_by(date)) %>%
    mutate(
        effective = real_income.x / real_income.y
    )

model2010s <-
    effective %>%
    filter(year(date) %in% 2013:2019) %>%
    lm(effective ~ date, data = .) %>% 
    broom::augment(
        newdata = tibble(date = seq(ymd(20140101), ymd(20320101), by = "year"))
    )

effective_g <-
    effective %>%
    ggplot(
        aes(x = date, y = effective)
    ) +
    geom_point(shape = 1, alpha = .7) +
    geom_line(
        data = model2010s, 
        aes(y = .fitted),
        linetype = "dashed"
    ) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    scale_x_date(
        breaks = seq(ymd(19700101), ymd(20300101), by = "10 years"),
        date_labels = "%Y",
    ) +
    geom_hline(
        yintercept = 1,
        linewidth = 2,
        alpha = .2
    ) +
    labs(
        x = "",
        y = "Effective Real personal income compared to model",
        title = paste0(
            "Real income (without government transfers) per working-age ",
            "person in the US as fraction of modeled amount"
        ),
        subtitle = "Model based on real income between 1977 and 2007",
    )

p <-
    realincome_g / effective_g +
    plot_annotation(
        caption = "Income in chained 2017 dollars\nSource: FRED W875RX1 and LFWA64TTUSM647S"
    )

ggsave("graphs/real_income_gap.png",
    width = 10, height = 10,
    plot = p
)
