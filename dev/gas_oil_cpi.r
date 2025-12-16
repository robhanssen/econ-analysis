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

approx(select_data$.fitted,select_data$DCOILWTICO,
    xout = c(2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5)
)

select_data %>%
    ggplot(aes(x = DCOILWTICO, y = .fitted)) +
    geom_line() +
    # geom_hline(
    #     yintercept = c(2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5),
    #     linetype = 2,
    #     color = "gray50"
    # ) +
    labs(
        x = "WTI Oil price ($/barrel)",
        y = "Predicted Gasoline price ($/gallon)"
    )

predgrid <-
    crossing(
        DCOILWTICO = seq(0, 150, 1),
        CPILFESL = seq(150, 400, 1)
    )

preddata <- broom::augment(
    mod,
    newdata = predgrid
)

labels <-
    tribble(
        ~CPILFESL, ~DCOILWTICO, ~.fitted,
        205, 5, 1.0,
        305, 5, 1.5,
        370, 10, 2.0,
        380, 25, 2.5,
        390, 40, 3.0,
        390, 55, 3.5,
        390, 70, 4.0,
        390, 88, 4.5,
        390, 102, 5.0,
        390, 118, 5.5
    ) %>%
    mutate(.fitted = scales::dollar(.fitted))


preddata %>%
    ggplot(aes(x = CPILFESL, y = DCOILWTICO, z = .fitted)) +
    geom_contour(
        breaks = seq(1, 7, .5),
        linetype = 1,
        linewidth = 2,
        color = "gray50",
        alpha = .5
    ) +
    geom_point(
        data = dat,
        aes(z = NULL),
        alpha = .1,
        shape = 1
    ) +
    geom_label(
        data = labels,
        aes(z = NULL, label = .fitted)
    ) +
    expand_limits(y = 0) +
    scale_x_continuous(limits = c(150, NA), breaks = seq(150, 400, 50)) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 200, 25)) +
    labs(x = "CPI value", y = "WTI Oil price ($/barrel)")



library(tidymodels)

dat_split <- initial_split(dat, strata = DCOILWTICO)

dat_train <- training(dat_split)
dat_test <- testing(dat_split)

lm_fit <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(GASREGW ~ DCOILWTICO * CPILFESL, data = dat_train)

mean_pred_train <- predict(lm_fit, new_data = dat_train)

rmse(
    bind_cols(dat_train, mean_pred_train),
    GASREGW,
    .pred
)

mean_pred_test <- predict(lm_fit, new_data = dat_test)

rmse(
    bind_cols(dat_test, mean_pred_test),
    GASREGW,
    .pred
)

lm_fit_full <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(GASREGW ~ DCOILWTICO * CPILFESL, data = dat)

mean_pred_full <- predict(lm_fit_full, new_data = dat)

rmse(
    bind_cols(dat, mean_pred_full),
    GASREGW,
    .pred
)

bind_cols(dat, mean_pred_full) %>%
    ggplot(aes(x = GASREGW, y = .pred)) +
    geom_point(alpha = .05, shape = 1) +
    geom_smooth(method = "lm", color = "red")
