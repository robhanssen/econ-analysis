library(tidyverse)
library(patchwork)
theme_set(theme_light() + 
    theme(
        panel.grid.minor.x = element_blank()
    )
    )

source("functions.r")

cpi <- retrieve_data("CPIAUCSL", "FRED") %>% 
    filter(date > "1960-01-01")


refdate_1 <- ymd(19680101)
refdate_2 <- ymd(20200401)

dist <- refdate_2 - refdate_1

refdata <- 
    cpi %>% filter(date == refdate_1)

cpi2020 <-
    cpi %>% filter(date >= refdate_2) %>%
    mutate(
        newdate = date - dist,
        newcpi = value / first(value) * refdata$value
    )


ggplot(cpi, aes(date, value)) + geom_line(color = "gray50", alpha = .8) + 
    geom_line(data = cpi2020,linewidth = 1.5,
            aes(x = newdate, y = newcpi),
     color = "red")  +
         geom_line(data = cpi2020,
            aes(x = date, y = value), linewidth = 1.5,
     color = "red")  +
     scale_y_log10() + 
     scale_x_date(
        # date_breaks = "10 year",
        date_labels = "%Y",
        breaks = ymd(19600101) + (0:100) * years(10)
     ) +
     labs(x = "", y = "CPI-U")