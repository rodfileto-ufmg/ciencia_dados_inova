library(OECD)
library(WDI)
library(tidyverse)
library(ggplot2)

getOption('timeout')
options(timeout=200)

#### Dados da OECD ####

dataset_OECD <- get_datasets()

df <- get_dataset("PATS_REGION")

df <- get_dataset("DUR_D")

df <- get_data_structure("DUR_D")

df <- get_dataset("PATS_IPC")

df <- get_dataset("PATS_IPC", 
                  filter = "EPO_A+USPTO_G+FAMILIES+PCT_A+EPO_G+USPTO_A+IP5.INVENTORS+APPLICANTS.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECD+WLD+NMEC+DZA+AND+ARG+ARM+BLR+BMU+BIH+BRA+BGR+CYM+CHN+HRV+CUB+CYP+DJI+ECU+EGY+SLV+GEO+GTM+HKG+IND+IDN+IRN+JAM+JOR+KAZ+KEN+PRK+KWT+LBN+LIE+MYS+MLT+MDA+MCO+MNG+MAR+MKD+NGA+PAK+PAN+PER+PHL+PRI+ROU+RUS+SAU+SYC+SGP+ZAF+LKA+TWN+THA+TTO+TUN+UKR+ARE+URY+UZB+VEN+ZWE+FRME+YUG.TOTAL+TOTAL_EST+BIOTECH+ICT+AI+NANOTECH+MEDICAL+PHARMA+ENV_TECH+WIPO_TECH+WIPO_11+WIPO_14+WIPO_17+WIPO_18+A61K+C12N+H.PRIORITY+APPLICATION+GRANT",
                  start_time = 2020, end_time = 2020, pre_formatted = TRUE)

# df <- get_dataset("PATS_REGION", 
#                   filter = "PCT_A.INVENTORS.AUS+AUT+BEL.TOTAL+BIOTECH+ICT",
#                   start_time = 2008, end_time = 2010, pre_formatted = TRUE)

# download.file("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/PATS_IPC/EPO_A+USPTO_G+FAMILIES+PCT_A+EPO_G+USPTO_A+IP5.INVENTORS+APPLICANTS.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECD+WLD+NMEC+DZA+AND+ARG+ARM+BLR+BMU+BIH+BRA+BGR+CYM+CHN+HRV+CUB+CYP+DJI+ECU+EGY+SLV+GEO+GTM+HKG+IND+IDN+IRN+JAM+JOR+KAZ+KEN+PRK+KWT+LBN+LIE+MYS+MLT+MDA+MCO+MNG+MAR+MKD+NGA+PAK+PAN+PER+PHL+PRI+ROU+RUS+SAU+SYC+SGP+ZAF+LKA+TWN+THA+TTO+TUN+UKR+ARE+URY+UZB+VEN+ZWE+FRME+YUG.TOTAL+TOTAL_EST+BIOTECH+ICT+AI+NANOTECH+MEDICAL+PHARMA+ENV_TECH+WIPO_TECH+WIPO_11+WIPO_14+WIPO_17+WIPO_18+A61K+C12N+H.PRIORITY+APPLICATION+GRANT/all?startTime=2005&endTime=2020", "temp.csv", quiet = FALSE)

# 
# https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/PATS_IPC/EPO_A+EPO_G+PCT_A+USPTO_A+USPTO_G+FAMILIES+IP5.INVENTORS+APPLICANTS.AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECD+WLD+ARG+BRA+BGR+CHN+CYP+HKG+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF+TWN.TOTAL+TOTAL_EST+BIOTECH+ICT+AI+NANOTECH+MEDICAL+PHARMA+BUILD+ENE+GHG+MAN+TRA+GOODS+WAT_WASTE.PRIORITY+APPLICATION+GRANT/all?startTime=2010&endTime=2020
# 




RD_expenditure <- get_dataset("GERD_SEO")

#### Dados do WDI ####

pesquisa_WDI <- WDIsearch(string = "article")

artigos <- WDI(country = "all", indicator = c("artigos" = "IP.JRN.ARTC.SC"), extra = TRUE)

distinct(artigos, income)




artigos_brasil <- artigos %>% filter(country == "Brazil") %>%
  filter(!(is.na(artigos)))

ggplot(artigos_brasil, aes(x = year, y = artigos)) + geom_line()


artigos_selecionados <- artigos %>% filter(country %in% c("Brazil", "United States")) %>%
  filter(!(is.na(artigos)))

ggplot(artigos_selecionados, aes(x = year, y = artigos, group = country, colour = country)) + geom_line()


#### Dados do IBGE ####

dir.create("dados/ibge")

library(sidrar)

muni <- search_sidra("territoriais")

muni <- as_tibble(muni)

pib_municipios <- info_sidra("5938")

variaveis <- tibble(codigo = pib_municipios$variable$cod, variavel = pib_municipios$variable$desc)

temp <- get_sidra("5938", geo = "City")

if (!(file.exists("./dados/ibge/area_territorial.xls"))) {

download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/2020/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2020.xls", "./dados/ibge/area_territorial.xls")

}

codigo_estados <- readxl::read_xls("./dados/ibge/area_territorial.xls") %>%
  distinct(CD_GCUF) %>%
  na.omit(CD_CGUF)

temp <- get_sidra("5938", geo = "City", geo.filter = list("State" = 21))

temp_1 <- map(codigo_estados$CD_GCUF, ~get_sidra("5938", geo = "City", geo.filter = list("State" = .x)))




temp_2 <- bind_rows(temp_1) %>%
  rename(codigo_variavel = "Variável (Código)") %>%
  filter(codigo_variavel %in% c("37", "543", "517")) %>%
  select(Município, Variável, Valor, "Unidade de Medida") %>%
  pivot_wider(names_from = c("Variável"), values_from = c("Valor"))


cor.test(temp_2$`Produto Interno Bruto a preços correntes`, temp_2$`Valor adicionado bruto a preços correntes da indústria`)

modelo_linear <- lm(`Produto Interno Bruto a preços correntes` ~ `Valor adicionado bruto a preços correntes da indústria`, data = temp_2)

summary(modelo_linear)

# temp_1 <- get_sidra(1378,
#                     variable = 93,
#                     geo = c("State","City"),
#                     geo.filter = list("Region" = 3, "Region" = 3),
#                     classific = c("c1"),
#                     category = list(1))


data_coleta <- sys.time


