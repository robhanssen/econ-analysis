library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())

source("functions.r")

gdp <- retrieve_data("GDPC1", "FRED")

gdp_plot <-
    gdp %>%
    mutate(floordate = floor_date(date, unit = "quarter")) %>%
    left_join(presidentinfo, by = "floordate") %>%
    fill(party, .direction = "down")


gdp_plot %>%
    filter(date > ymd("1979-12-31")) %>%
    ggplot() +
    aes(date, value, color = party, group = TRUE) +
    geom_line() +
    scale_y_log10(
        breaks = c(1, 2, 5) * 10^rep(1:10, each = 3),
        limits = c(2000, NA),
        labels = scales::dollar_format(accurary = 1)
    ) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(
        x = "Date",
        y = "US GDP (in billions of USD)",
        caption = "Dashed vertical line indicate inauguration dates",
        title = "US GDP since 1980"
    ) +
    scale_color_manual(values = partycolor) +
    theme(legend.position = "none")


#
# Obama to Trump transition
#
cutoff_date_lo <- ymd("2009-09-01")
cutoff_date_hi <- as_datetime("2017-01-21")

gdpmodel <-
    gdp %>%
    ungroup() %>%
    # rename(date = "floordate") %>%
    filter(date > cutoff_date_lo, date < cutoff_date_hi) %>%
    nest() %>%
    mutate(gdpmodel = map(data, function(tbl) lm(log10(value) ~ date, data = tbl))) # nolint

rsq <- gdpmodel %>%
    mutate(modelq = map(gdpmodel, function(x) x %>% broom::glance())) %>% # nolint
    unnest(modelq) %>%
    pull(r.squared)

modelconstant <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% tidy()
    )) %>%
    unnest(modelq) %>%
    filter(term == "date") %>%
    pull(estimate)

pct_increase <- scales::percent(10^(modelconstant * 365) - 1, accuracy = .1)

ten_years <- 86400 * 365 * 10

gdp_predict_range <-
    gdp %>%
    ungroup() %>%
    # rename(date = "floordate") %>%
    filter(date > cutoff_date_lo) %>%
    select(date)



predictions <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% augment(interval = "prediction", newdata = gdp_predict_range) # nolint
    )) %>%
    unnest(modelq) %>%
    mutate(across(.cols = .fitted:.upper, .fns = ~ 10^.x))

gdp_max_pred <- predictions %>%
    filter(date < ymd("2019-11-10")) %>%
    slice_max(date, n = 1) %>%
    select(.fitted) %>%
    pull(.fitted)

gdp_max_real <- gdp %>%
    filter(date < ymd("2019-11-05")) %>%
    slice_max(date, n = 1) %>%
    pull(value)

real_growth <- scales::dollar(-gdp_max_pred + gdp_max_real,
    accuracy = 1,
    suffix = "B"
)

real_growth_per <- scales::percent((-gdp_max_pred + gdp_max_real) / gdp_max_real, # nolint
    accuracy = .01
)

max_date <- max(gdp$date)
comment <- paste0(
    "Difference between 2010s model\nand outcome : ",
    real_growth,
    " (", real_growth_per, ")"
)

gdp_full_scale <-
    predictions %>%
    ggplot() +
    aes(x = date, y = .fitted) +
    geom_line(lty = 2) +
    geom_point(
        data = gdp %>%
            filter(date > cutoff_date_lo),
        aes(x = date, y = value)
    ) +
    scale_x_date(date_break = "2 year", date_label = "%Y") +
    scale_y_continuous(limit = c(14000, NA)) +
    geom_line(aes(y = .lower), color = "gray50", lty = 1) +
    geom_line(aes(y = .upper), color = "gray50", lty = 1) +
    geom_vline(xintercept = ymd("2017-01-21"), lty = 2, color = "gray50") +
    geom_vline(xintercept = ymd("2021-01-21"), lty = 2, color = "gray50") +
    annotate("label",
        x = ymd("2014-06-01"),
        y = 14000, color = "red",
        label = "training set"
    ) +
    annotate("label",
        x = ymd("2018-09-01"),
        y = 14000,
        color = "darkgreen",
        label = "prediction"
    ) +
    # annotate("label",
    #     x = ymd("2018-06-01"),
    #     y = 20000,
    #     label = comment
    # ) +
    annotate("label",
        x = ymd("2014-11-01"),
        y = 18000,
        label = paste0(pct_increase, "\nr.sq = ", scales::pvalue(rsq))
    ) +
    labs(
        x = "Date",
        y = "Real GDP (in billions of $)",
        caption = "Model trained between 2009 and 2017 and predicted into today"
    )

# ggsave("quantmod-variants/us-gdp-since-1980.png",
#     width = 6,
#     height = 6,
#     plot = gdp_allyears
# )

#
# difference plot
#

gdp_predict_range <-
    gdp %>%
    ungroup() %>%
    filter(date > cutoff_date_lo) %>%
    select(date)


predictions_diff <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% augment(interval = "prediction", newdata = gdp_predict_range) # nolint
    )) %>%
    unnest(modelq) %>%
    mutate(across(.cols = .fitted:.upper, .fns = ~ 10^.x)) %>%
    inner_join(gdp, by = c("date")) %>%
    mutate(
        real_growth = value - .fitted,
        real_lo = .fitted - .lower,
        real_hi = .fitted - .upper
    )

max_date <- max(gdp$date)

max_dip <- min(predictions_diff$real_growth)
dip_comment <- paste0(
    "COVID-19 GDP dip\n",
    scales::dollar(-max_dip, accuracy = 1, suffix = "B"),
    " below 2010s model"
)

gdp_diff_scale <-
    predictions_diff %>%
    ggplot() +
    aes(x = date, y = real_growth) +
    geom_point() +
    scale_x_date(breaks = "2 year", date_label = "%Y") +
    scale_y_continuous(limits = c(-3000, 1500)) +
    geom_line(aes(y = real_lo), color = "gray70", lty = 1) +
    geom_line(aes(y = real_hi), color = "gray70", lty = 1) +
    geom_vline(xintercept = ymd("2009-01-21"), lty = 2, color = "gray50") +
    geom_vline(xintercept = ymd("2017-01-21"), lty = 2, color = "gray50") +
    geom_vline(xintercept = ymd("2021-01-21"), lty = 2, color = "gray50") +
    annotate("label",
        x = ymd("2014-06-01"),
        y = -500,
        color = "red",
        label = "training set"
    ) +
    annotate("label",
        x = ymd("2018-09-01"),
        y = -500,
        color = "darkgreen",
        label = "prediction"
    ) +
    # annotate("label",
    #     x = ymd("2018-01-01"),
    #     y = 900,
    #     label = comment
    # ) +
    # annotate("label",
    #     x = ymd("2019-09-01"),
    #     y = -2500,
    #     label = dip_comment
    # ) +
    labs(
        x = "Date",
        y = "Real GDP growth above 2010s model (in $B)",
        caption = "Model trained between 2009 and 2017 and predicted into today"
    )

p <- gdp_full_scale + gdp_diff_scale

ggsave("graphs/real-gdp-growth.png", width = 12, height = 6, plot = p)

#
#
#
#

cutoff_date_lo <- ydm("2020-05-01")
cutoff_date_hi <- today()

gdpmodel <-
    gdp %>%
    ungroup() %>%
    filter(date > cutoff_date_lo, date < cutoff_date_hi) %>%
    nest() %>%
    mutate(gdpmodel = map(data, function(tbl) lm(log10(value) ~ date, data = tbl))) # nolint

rsq <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% glance()
    )) %>%
    unnest(modelq) %>%
    pull(r.squared)


model_constant_pre2020 <- modelconstant

modelconstant <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% tidy()
    )) %>%
    unnest(modelq) %>%
    filter(term == "date") %>%
    pull(estimate)

pct_increase <- scales::percent(10^(modelconstant * 365) - 1, accuracy = .1)

predict_post_2020 <-
    gdpmodel %>%
    mutate(modelq = map(
        gdpmodel,
        function(x) x %>% augment(interval = "confidence")
    )) %>%
    unnest(modelq) %>%
    mutate(across(.cols = .fitted:.upper, .fns = ~ 10^.x))

model_contant_inc <- 10^(365 * (modelconstant - model_constant_pre2020)) - 1
model_contant_comment <- paste0(
    "Estimate inflation: ", scales::percent(model_contant_inc, accuracy = .1),
    " over 2010s model"
)


# gdp2 <- gdp_full_scale +
#     geom_line(
#         data = predict_post_2020,
#         aes(y = .fitted),
#         lty = 2,
#         color = "gray50"
#     ) +
#     annotate("label",
#         x = ymd("2020-01-01"),
#         y = 24000,
#         label = paste0(pct_increase, "\nr.sq = ", scales::pvalue(rsq))
#     ) +
#     labs(subtitle = model_contant_comment)

# ggsave("graphs/estimating-real-inflation.png",
#     width = 6,
#     height = 6,
#     plot = gdp2
# )
# p <- gdp2 + gdp_diff_scale

# ggsave("graphs/real-gdp-growth2.png", width = 12, height = 6, plot = p)

#
# GDP growth
#

gdp %>%
    arrange(date) %>%
    mutate(month = month(date), year = year(date)) %>%
    group_by(month) %>%
    mutate(gdp_growth = value / lag(value) - 1) %>%
    ungroup() %>%
    drop_na() %>%
    filter(date > ymd(19791231)) %>%
    ggplot() +
    aes(date, gdp_growth) +
    geom_line() +
    labs(x = "", y = "Annual real GDP growth (in %)") +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_percent()) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    geom_hline(yintercept = 0, lty = 1, alpha = .3, linewidth = 1)

full_join(presidentinfo, gdp, by = c("inaugdate" = "date")) %>%
    rename(date = inaugdate) %>%
    arrange(date) %>%
    fill(c("president", "party"), .direction = "down") %>%
    select(-floordate) %>%
    drop_na() %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    mutate(gdp_growth = value / lag(value) - 1) %>%
    ungroup() %>%
    drop_na() %>%
    filter(date > ymd(19791231)) %>% 
    ggplot() +
    aes(date, gdp_growth, color = party, group = TRUE) +
    geom_line(show.legend = FALSE) +
    labs(x = "", y = "Annualized real GDP growth (in %)") +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_percent()) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    geom_hline(yintercept = 0, lty = 1, alpha = .3, linewidth = 1) +
    scale_color_manual(values = partycolor)