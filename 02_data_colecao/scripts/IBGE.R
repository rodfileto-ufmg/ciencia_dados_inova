library(tidyverse)
library(sidrar)
library(readxl)
library(readODS)


#### Dados do IBGE ####

# Criar diretório


muni <- search_sidra("territoriais")

muni <- as_tibble(muni)

pib_municipios <- info_sidra("5938")

variaveis <- tibble(codigo = pib_municipios$variable$cod, variavel = pib_municipios$variable$desc)

temp <- get_sidra("5938", geo = "City")

# Alterar diretório de destino
# download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/2021/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xls",
#               destfile = "./02_data_colecao/dados/area_territorial.xls")
# 
# download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/2021/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.ods",
#               destfile = "./02_data_colecao/dados/area_territorial.ods")


codigo_estados <- read_ods("./02_data_colecao/dados/area_territorial.ods") %>%
  distinct(CD_UF, .keep_all = TRUE) %>%
  filter(!(is.na(CD_UF) | grepl("OBS", CD_UF)))

MA <- get_sidra("5938", geo = "City", geo.filter = list("State" = 21))

todos_estados <- map_dfr(codigo_estados$CD_UF[1:3], ~get_sidra("5938", geo = "City", geo.filter = list("State" = .x)))

dados_wide <- todos_estados %>%
  rename(codigo_variavel = "Variável (Código)") %>%
  filter(codigo_variavel %in% c("37", "543", "517")) %>%
  select(Município, Variável, Valor, "Unidade de Medida") %>%
  pivot_wider(names_from = c("Variável"), values_from = c("Valor"))


cor.test(temp_2$`Produto Interno Bruto a preços correntes`, temp_2$`Valor adicionado bruto a preços correntes da indústria`)

modelo_linear <- lm(`Produto Interno Bruto a preços correntes` ~ `Valor adicionado bruto a preços correntes da indústria`, data = dados_wide)

summary(modelo_linear)

data_coleta <- format(as.Date(Sys.time(),format="%Y-%m-%d"), format = "%d/%m/%Y")

write_lines(data_coleta, "data_coleta.txt")
