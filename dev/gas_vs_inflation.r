library(tidyverse)

theme_set(
    theme_light() +
        theme(
            # axis.line = element_blank(),
            # panel.grid = element_blank(),
            # panel.border = element_blank()
        )
)

source("functions.r")

dat <-
    retrieve_data(c("CPILFESL", "GASREGW", "DCOILWTICO")) %>%
    pivot_wider(values_from = "value", names_from = "index") %>%
    arrange(date) %>%
    mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    fill(c("GASREGW", "CPILFESL", "DCOILWTICO"), .direction = "down") %>%
    filter(date >= "1992-01-20")

recessions_map <- retrieve_data("JHDUSRGDPBR") %>%
    select(-index) %>%
    # use clearer name for the change in value
    mutate(delta = value - lag(value)) %>%
    filter(!is.na(delta) & delta != 0) %>%
    # label each non-zero change as a recession "start" or "stop"
    mutate(period = ifelse(delta > 0, "start", "stop")) %>%
    select(-value, -delta) %>%
    pivot_wider(names_from = period, values_from = date)


recessions <- tibble(
    start = as.Date(unlist(recessions_map$start)),
    end = as.Date(unlist(recessions_map$stop))
) %>%
    filter(start > as.Date("1990-01-01"))


dat %>%
    mutate(
        expected_gas_price = first(GASREGW) * (CPILFESL / first(CPILFESL))
    ) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = GASREGW), color = "blue") +
    geom_line(aes(y = expected_gas_price), color = "red") +
    scale_y_continuous(
        name = "Gasoline Price ($/gallon)",
        # sec.axis = sec_axis(~ . * 100, name = "CPI (1982-84=100)")
    ) +
    labs(
        title = "Gasoline Prices and scaled CPI",
        x = "Date"
    ) +
    geom_rect(
        data = recessions,
        inherit.aes = FALSE,
        aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill = "grey",
        alpha = 0.5
    ) +
    geom_text(
        data = tibble(
            date = as.Date(c("2008-09-01", "2020-03-01", "2016-01-01")),
            label = c("Great\nRecession", "COVID-19\nRecession", "Oil glut"),
            y = c(1.3, 1.4, 1.5)
        ), aes(x = date, y = y, label = label)
    ) +
    geom_text(
        data = tibble(
            date = as.Date(c("2017-09-01", "2024-08-01")),
            label = c("Real price", "scaled CPI\nfrom 1992"),
            y = c(3.1, 1.95), color = c("blue", "red")
        ), aes(x = date, y = y, label = label, color = I(color))
    )


mod <-
    lm(GASREGW ~ DCOILWTICO * CPILFESL, data = dat)

rsq <- scales::pvalue(summary(mod)$adj.r.squared)

formula <- "lm(GASREGW ~ DCOILWTICO * CPILFESL"

str(summary(mod))

broom::augment(mod) %>%
    ggplot(aes(x = GASREGW, y = .fitted)) +
    geom_point(alpha = .15, shape = 1) +
    geom_smooth(method = "lm")


select_data <-
    broom::augment(
        mod,
        newdata = tibble(
            DCOILWTICO = seq(0, 120, 1),
            CPILFESL = last(dat$CPILFESL)
        ),
        interval = "prediction"
    )


data_source_limits <-
    with(
        dat,
        c(min(date), max(date))
    ) %>% year()

# approx(select_data$.fitted,select_data$DCOILWTICO,
#     xout = c(2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5)
# )


target_price <- 2.00

two_dollar_limits <- map_dbl(
    c(".lower", ".fitted", ".upper"),
    \(m) approx(select_data[[m]], select_data$DCOILWTICO, xout = target_price)$y
) %>% sort()


limits <- last(dat$CPILFESL) * c(0.90, 1, 1.10)

select_data %>%
    ggplot(aes(x = DCOILWTICO, y = .fitted)) +
    geom_line() +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) +
    geom_point(
        data = dat %>% filter(CPILFESL < limits[3], CPILFESL > limits[1]),
        aes(x = DCOILWTICO, y = GASREGW),
        inherit.aes = FALSE,
        alpha = 0.5, shape = 21
    ) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 0.01)) +
    labs(
        x = "WTI Oil price ($/barrel)",
        y = "Predicted Gasoline price ($/gallon)",
        title = "The MAGA Misconception: Why We'll Never Have $2 Gas Again",
        caption = glue::glue(
            "Model of gasoline prices based on oil price and chained inflation index on data from {paste(data_source_limits, collapse = ' - ')}",
            " ({formula}; R<sup>2</sup> = {rsq}).",
            "<BR/>Prediction is based on CPILFESL = {round(limits[2],1)}. ",
            "Selected data points are those with CPI within 10% of the last value ({round(limits[1], 1)} - {round(limits[3], 1)}).",
            "<BR/>Data source: Federal Reserve Economic Data (FRED)."
        )
    ) +
    annotate(
        geom = "segment",
        x = two_dollar_limits[1], xend = two_dollar_limits[3], y = target_price, yend = target_price,
        color = "red", linewidth = 1, alpha = .8,
        arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "both")
    ) +
    annotate(geom = "point", x = two_dollar_limits[2], y = target_price, color = "red", size = 3) +
    annotate(
        geom = "text", y = target_price, x = two_dollar_limits, label = scales::dollar(two_dollar_limits),
        vjust = c(2, -1, 2), hjust = 0.5, color = "red"
    ) +
    theme(
        plot.caption.position = "plot",
        plot.caption = ggtext::element_markdown(hjust = 0, size = 8, color = "gray30"),
        plot.title.position = "plot",
        plot.title = ggtext::element_markdown(size = 16)
    )

ggsave("dev/gas_vs_inflation.png", width = 8, height = 5)
