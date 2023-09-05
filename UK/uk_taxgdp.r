library(tidyverse)


taxrev <-
    read_csv("UK/taxrev-uk.csv") %>%
    filter(TAX == "TOTALTAX", GOV == "NES") %>%
    select(year = YEA, taxrev = Value) %>%
    mutate(taxrev = taxrev / 1000)


gbpusd <- quantmod::getSymbols("DEXUSUK", src = "FRED", auto.assign = FALSE)

exch <-
    as_tibble(gbpusd) %>%
    janitor::clean_names() %>%
    mutate(date = zoo::index(gbpusd)) %>%
    mutate(year = year(date)) %>%
    summarize(
        mean_ex = mean(dexusuk, na.rm = TRUE),
        .by = "year"
    )

ukgdp <-
    read_csv("UK/gdp-uk.csv") %>%
    janitor::clean_names() %>%
    mutate(year = year(date), gdp_usd = gdp_billions_of_us / 1000) %>%
    select(year, gdp_usd)


taxdata <- inner_join(
    taxrev, exch,
    by = "year"
) %>%
    inner_join(
        ukgdp,
        by = "year"
    ) %>%
    mutate(
        gdp = gdp_usd / mean_ex,
        tax_rate = taxrev / (gdp * 1000),
        tax_rate_usd = taxrev / (gdp_usd * 1000)
    )

l <- quantile(taxdata$tax_rate, pnorm(-2:2))

taxdata %>%
    ggplot(aes(x = year, y = tax_rate)) +
    geom_point(shape = 1) + 
    scale_y_continuous(limits = c(0.2, 0.4),
        labels = scales::percent_format()) +
    # geom_line(aes(y = tax_rate_usd), color = "gray70") +
    labs(x = "", y = "Ratio of tax income to GDP\n(in GBP/GBP)",
        caption = "Lines indicate mean, 1sigma and 2sigma above and below") + 
    geom_hline(yintercept = l,
        alpha = .5,
        lty = c(3,2,1,2,3)) + 
    theme_light()

ggsave("UK/taxrev_gdp_ratio.png", width = 7, height = 4)

tibble(rate = seq(.25, .4, .005),
        cdf = map_dbl(rate, 
    ~ taxdata %>% filter(tax_rate <= .x) %>% nrow(.))
) %>%
    ggplot(aes(x = rate, y= cdf / max(cdf))) + 
    geom_line()





p <- lm(log10(gdp) ~ year, data = taxdata %>% filter(year %in% 2010:2019)) %>% 
    broom::augment(newdata = tibble(year = 2010:2023))

p2 <- lm(log10(gdp) ~ year, data = taxdata %>% filter(year %in% 1995:2008)) %>% 
    broom::augment(newdata = tibble(year = 1995:2023))



taxdata %>%
    # filter(year >= 2000) %>%
    ggplot(aes(x = year, y = gdp)) + 
    geom_point() + 
    geom_line(data = p, aes(y = 10^.fitted)) +
    geom_line(data = p2, aes(y = 10^.fitted)) 