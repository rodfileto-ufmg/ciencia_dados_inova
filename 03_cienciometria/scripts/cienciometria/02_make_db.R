library(bibliometrix)
library(tidyverse)

arquivos <- list.files(path = c("./03_cienciometria/dados/"), pattern = "savedrecs.*", full.names = TRUE)

dados_wos <- convert2df(arquivos, dbsource = "wos", format = "plaintext") %>%
  select(UT, TI, AB, DE, ID, CR, PY, TC, SC, SO, JI, DI, DT, AU, C1, VL, PG, SR, WC) %>%
  distinct(UT, .keep_all = TRUE)

write_csv(dados_wos, "./03_cienciometria/dados/core.csv")
