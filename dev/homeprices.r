library(tidyverse)
library(forecast)

theme_set(
    theme_light() +
        theme(
            axis.line = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank()
        )
)

source("functions.r")

dat <-
    retrieve_data(c("MSPUS", "MEHOINUSA646N")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    arrange(date) %>%
    rename(homeprice = MSPUS, income = MEHOINUSA646N) %>%
    mutate(income = zoo::na.approx(income, na.rm = FALSE)) %>%
    mutate(ratio = homeprice / income) %>%
    drop_na()

# x <- ts(data = dat$ratio, start = c(1984,1), frequency = 4)


ts <- ts(dat$ratio, frequency = 4, start = c(year(first(dat$date)), month(first(dat$date))))
x <- forecast::ets(ts, model = "MAA")

forecast::autoplot(x) +
    scale_x_continuous() +
    scale_y_continuous(
        label = scales::label_number()
    )

ts %>%
    ets(model = "MAA") %>%
    # forecast() %>%
    autoplot()


