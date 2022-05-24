library(sparklyr)
library(arrow)
library(tidyverse)

# options(sparklyr.console.log = TRUE)

# Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre/")

# Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/")

# spark_dir = "~/spark_temp"

config <- spark_config()
# 
# config$`sparklyr.shell.driver-java-options` <-  paste0("-Djava.io.tmpdir=", spark_dir)
# 
# config$`sparklyr.shell.driver-class-path` <- "/media/mynewdrive/scielo/postgresql-42.2.16.jar"

#### Configuração de memória ####

config$`sparklyr.shell.driver-memory` <- "4G"
config$`sparklyr.shell.executor-memory` <- "4G"
config$spark.yarn.executor.memoryOverhead <- "2G"


#### Conectando ao cluster ####
sc <- spark_connect(master = "local", config = config)

#### Exemplo de leitura de arquivos parquet ####

pagamentos <- spark_read_parquet(sc, name = "pagamentos", path = "./data/pagamentos_data", header = TRUE, charset = "ISO-8859-1")

empenhos <- spark_read_parquet(sc, name = "empenhos", path = "./data/despesas_empenho_parquet", header = TRUE, charset = "ISO-8859-1")

#### Exemplo de leitura de arquivos csv ####

pagamentos_empenhos_impactados <- spark_read_csv(sc, name = "pagamento_empenhosimpactados", path = "./data/despesas_pagamento_empenhosimpactados", header = TRUE, delimiter = ";", charset = "ISO-8859-2")

#### Enviado dados locais para o cluster ####

CNPJ <- read_delim("./data/CNPJ/202102_CNPJ.csv", delim = ";", locale = readr::locale(encoding = "latin1")) %>%
  sparklyr::copy_to(sc,., name = "CNPJ")

CNAE <- read_delim("./data/CNPJ/202102_CNAE.csv", delim = ";", locale = readr::locale(encoding = "latin1")) %>%
  sparklyr::copy_to(sc,., name = "CNAE")

#### Extraindo encomenda tecnologica ####

encomenda_tec <- empenhos %>%
  select(-Data_Emissao) %>%
  filter(Modalidade_de_Licitacao == "Dispensa de Licitação") %>%
  mutate(Referencia_de_Dispensa_ou_Inexigibilidade = regexp_replace(Referencia_de_Dispensa_ou_Inexigibilidade, "(?i)(?<=\\\\d)(?=[a-z])|(?<=[a-z])(?=\\\\d)", ' ')) %>% # https://stackoverflow.com/questions/48963908/how-to-insert-space-between-alphabet-characters-and-numeric-character-using-rege
  filter(Referencia_de_Dispensa_ou_Inexigibilidade == "ART 24/31 LEI 8666/93") %>%
  inner_join(pagamentos_empenhos_impactados, by = "Codigo_Empenho") %>%
  inner_join(pagamentos_data, by = "Codigo_Pagamento") %>%
  mutate(ano = Substr(Data_Emissao, -4)) %>%
  mutate(Valor_Pago_R = regexp_replace(Valor_Pago_R, ",", ".")) %>%
  mutate(Valor_Pago_R = as.numeric(Valor_Pago_R)) %>%
  sdf_register("encomenda_tec")

sdf_persist(encomenda_tec, storage.level = "MEMORY_AND_DISK")
# 