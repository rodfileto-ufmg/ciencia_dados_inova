library(WDI)
library(tidyverse)
library(ggplot2)

#### Dados do WDI ####

pesquisa_WDI <- WDIsearch(string = "article")

artigos <- WDI(country = "all", indicator = c("artigos" = "IP.JRN.ARTC.SC"), extra = TRUE)

distinct(artigos, income)

artigos_brasil <- artigos %>% filter(country == "Brazil") %>%
  filter(!(is.na(artigos)))

artigos_selecionados <- artigos %>% filter(country %in% c("Brazil", "United States")) %>%
  filter(!(is.na(artigos)))

# Informações sobre o GGPLOT2
# https://ggplot2.tidyverse.org/

ggplot(artigos_brasil, aes(x = year, y = artigos)) + geom_line() + theme_classic() + labs(x = "", y = "") + 
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

ggplot(artigos_selecionados, aes(x = year, y = artigos, group = country, colour = country)) + geom_line() +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) + 
  theme_classic() + labs(x = "", y = "", colour = "")

# ggsave()
  

