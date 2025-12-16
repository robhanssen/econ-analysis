library(tidyverse)

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
    retrieve_data(c("DCOILWTICO", "CPILFESL", "GASREGW")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    filter(DCOILWTICO > 0) %>%
    arrange(date) %>%
    mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    fill(c("DCOILWTICO", "GASREGW", "CPILFESL"), .direction = "downup") %>%
    filter(date > "1992-01-01")

mod <-
    lm(GASREGW ~ DCOILWTICO * CPILFESL, data = dat)

broom::augment(mod) %>%
    ggplot(aes(x = GASREGW, y = .fitted)) +
    geom_point(alpha = .15, shape = 1) +
    geom_smooth(method = "lm")


select_data <-
    tibble(
        DCOILWTICO = seq(10, 100, 1),
        CPILFESL = last(dat$CPILFESL)
    ) %>%
    broom::augment(
        mod,
        newdata = .
    )

# approx(select_data$.fitted,select_data$DCOILWTICO,
#     xout = c(2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5)
# )

select_data %>%
    ggplot(aes(x = DCOILWTICO, y = .fitted)) +
    geom_line() +
    labs(
        x = "WTI Oil price ($/barrel)",
        y = "Predicted Gasoline price ($/gallon)"
    )

