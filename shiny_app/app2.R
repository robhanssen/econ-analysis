library(shiny)
library(tidyverse)

# Use the functions in the repo root (adjust if running from a different cwd)
source("functions.r")

ui <- fluidPage(
    titlePanel("Predicted Gas Price vs WTI Oil Price (by CPI)"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "cpi", "CPI range",
                min = 200, max = 400,
                value = last(retrieve_data("CPILFESL")$value), step = 0.1
            ),
            sliderInput("oil_range", "Oil price range ($/barrel)", min = 10, max = 100, value = c(10, 100)),
            actionButton("refit", "Re-fit model / Refresh data")
        ),
        mainPanel(
            plotOutput("gasPlot", height = "480px"),
            verbatimTextOutput("modelInfo")
        )
    )
)

server <- function(input, output, session) {
    # Fit model and cache data on startup or when user clicks "refit"
    model_obj <- reactiveVal(NULL)

    fit_model <- function() {
        dat <-
            retrieve_data(c("DCOILWTICO", "CPILFESL", "GASREGW")) %>%
            pivot_wider(values_from = "value", names_from = "index") %>%
            filter(DCOILWTICO > 0) %>%
            arrange(date) %>%
            mutate(across(!date, ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
            fill(c("DCOILWTICO", "GASREGW", "CPILFESL"), .direction = "downup") %>%
            filter(date > "1992-01-01")

        mod <- lm(GASREGW ~ DCOILWTICO * CPILFESL, data = dat)

        model_obj(list(dat = dat, mod = mod))
    }

    # initial fit
    observeEvent(TRUE,
        {
            # Fit once when app starts
            fit_model()
        },
        once = TRUE
    )

    # re-fit on button
    observeEvent(input$refit, {
        fit_model()
    })

    # update CPI input to last observed CPI after model fit
    observeEvent(model_obj(), {
        mo <- model_obj()
        if (!is.null(mo)) {
            last_cpi <- last(mo$dat$CPILFESL)
            updateNumericInput(session, "cpi", value = as.numeric(last_cpi))
        }
    })

    select_data <- reactive({
        mo <- model_obj()
        req(mo)

        cpi_val <- input$cpi
        if (is.null(cpi_val) || is.na(cpi_val)) cpi_val <- input$cpi

        oil_seq <- seq(input$oil_range[1], input$oil_range[2], by = 1)
        newdata <- tibble(DCOILWTICO = oil_seq, CPILFESL = cpi_val)
        preds <- predict(mo$mod, newdata = newdata)

        tibble(DCOILWTICO = oil_seq, PredictedGas = preds)
    })

    output$gasPlot <- renderPlot({
        sd <- select_data()
        mo <- model_obj()
        req(sd, mo)

        ggplot(sd, aes(x = DCOILWTICO, y = PredictedGas)) +
            geom_line(color = "steelblue", linewidth = 1) +
            labs(
                x = "WTI Oil price ($/barrel)",
                y = "Predicted Gasoline price ($/gallon)",
                title = paste0("Predicted gas price at CPI = ", formatC(input$cpi, digits = 2, format = "f"))
            ) +
            theme_minimal()
    })

    # output$modelInfo <- renderPrint({
    #     mo <- model_obj()
    #     req(mo)
    #     summary(mo$mod)
    # })
}

shinyApp(ui, server)
