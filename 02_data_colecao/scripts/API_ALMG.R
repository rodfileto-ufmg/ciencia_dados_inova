library(httr)
library(jsonlite)
library(tidyverse)

# Maximo tamanho de pagina = 100


# Definir o tipo de proposição a ser extraída conforme nomenclatura disponível em http://dadosabertos.almg.gov.br/ws/proposicoes/ajuda#tiposProposicao
tipo <- "PL"

dir.create("./02_data_colecao/dados/ALMG/")

dir.create(paste0("./02_data_colecao/dados/ALMG/", tipo))

ano <- 1980

# Fazer para cada ano
baixar_base <- function(ano) {
  
  #### listar numero de resultados
  
  url <- paste0("http://dadosabertos.almg.gov.br/ws/proposicoes/pesquisa/direcionada?ano=", ano, "&sitTram=2&tipo=", tipo, "&formato=json&p=1") # endere?o de lista de projetos de lei
  
  nr_resultados <- GET(url)
  
  nr_resultados <- rawToChar(nr_resultados$content) %>%
    fromJSON(flatten=TRUE) %>%
    .$resultado %>%
    .$noDocumentos %>%
    as.numeric()
  
  nr_paginas <- ceiling(nr_resultados/100)
  

  #### extrair dados para cada página 

  if (length(nr_paginas) != 0) {
    
    extrair_paginas <- function(pagina) {
      
      pagina <- 1
      
      url_pagina <- paste0("http://dadosabertos.almg.gov.br/ws/proposicoes/pesquisa/direcionada?ano=", ano, "&sitTram=2&tipo=", tipo, "&formato=json&tp=100&p=", pagina) # endereçoo de lista de projetos de lei
      
      dados_json <- GET(url_pagina)
      
      if(dados_json$status_code == 200) {
        
        
        tabela <- content(dados_json, "text", encoding = "UTF-8") %>%
          fromJSON(flatten=TRUE)
        
        tabela <- as_tibble(tabela$resultado$listaItem) %>%
          mutate_all(as.character)
        
        Sys.sleep(5)
        
      } else {
        
        tabela <- tibble(dominio = as.character(pagina), ano = as.character(ano))
        
      }
      
      Sys.sleep(3)
      
      return(tabela)
      
    }
    
    tabela <- map_dfr(1:nr_paginas, extrair_paginas) %>%
      bind_rows()
    
    write_csv(tabela, paste0("./02_data_colecao/dados/ALMG/", tipo, "/", ano, ".csv"))
    
    return(tabela)
    
  }
}


dados <- map(1980, baixar_base) # Pegar dados para os anos desejados - Ex: 2019:2021 extrai os dados considerado o período de 2019 a 2021

#### Processamento básico dos arquivos

arquivos <- list.files("./02_data_colecao/dados/ALMG/PL", full.names = TRUE)

PL <- map(arquivos, read_csv) %>%
  bind_rows()

#### extrair partido do autor

PL <- PL %>%
  select(dominio, autor, numero, ano, ementa, assuntoGeral) %>%
  separate_rows(autor, sep = "\n") %>% # Múltipla autoria separada por nova linha \n
  separate_rows(assuntoGeral, sep = "\n") %>% # Múltiplo assuntos separados por nova linha \n
  mutate(partido = sub(".*   ", "", autor)) %>%
  mutate(autor = sub("   .*", "", autor))


#### extrair temas

# Partidos por assunto

partidos_assunto <- PL %>%
  count(partido, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

deputado_assunto <- PL %>%
  count(autor, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

assunto <- PL %>%
  count(assuntoGeral, sort = TRUE)


#### Converter período 2019-2021 para excel ####

# arquivos <- list.files("./dados/dados_brutos/PL", full.names = TRUE)
# 
# arquivos <- arquivos[grepl("2019|2020|2021", arquivos)]
# 
# temp <- map(arquivos, read_csv) %>%
#   bind_rows()
# 
# 
# write.xlsx(temp, "./dados/PL_2019_2021.xlsx")







