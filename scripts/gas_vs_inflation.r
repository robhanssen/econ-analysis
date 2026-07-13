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

mod <-
    lm(GASREGW ~ DCOILWTICO * CPILFESL, data = dat)

rsq <- scales::pvalue(summary(mod)$adj.r.squared)

formula <- "lm(GASREGW ~ DCOILWTICO * CPILFESL"

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


target_price <- 2.00

two_dollar_limits <- map_dbl(
    c(".lower", ".fitted", ".upper"),
    \(m) approx(select_data[[m]], select_data$DCOILWTICO, xout = target_price)$y
) %>% sort()


limits <- last(dat$CPILFESL) * c(0.90, 1, 1.10)

gas_model_g <- 
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

ggsave("graphs/gas_vs_inflation.png", width = 8, height = 5, plot = gas_model_g)
