library(tidyverse)
library(visNetwork)
library(igraph)
library(highcharter)

# insumos utilizados na produção de cada atividade registrados nas colunas

# Carregar tabela com codigo dos produtos
cod_produtos <- read_csv("03_cienciometria/dados/cod_produtos.csv") %>%
  mutate(id = 1:nrow(.)) %>% # criando coluna de id para visnetwork
  rename(label = desc_produto_20) %>% # criando coluna label para visnetwork
  select(id, label) # inverter ordem colunas para igraph

# Carregar tabela insumo-produto
insumo_produto_20 <- read_csv("03_cienciometria/dados/insumo_produto_nivel_20.csv", col_names = FALSE) %>%
  mutate(across(everything(), ~ as.numeric(gsub(" ", "", .x, fixed = TRUE)))) %>%
  as.matrix()

# Atribuir nome das categorias como colunas
colnames(insumo_produto_20) <- cod_produtos$label

# Atribuir nome das categorias como linhas
rownames(insumo_produto_20) <- cod_produtos$label

# Extrair triângulo superior da matrix
insumo_produto_20_superior <- insumo_produto_20
insumo_produto_20_superior[!(upper.tri(insumo_produto_20_superior, diag = FALSE))] <- 0


# Extrair triângulo inferior da matrix

insumo_produto_20_inferior <- insumo_produto_20

insumo_produto_20_inferior[!(lower.tri(insumo_produto_20_inferior, diag = FALSE))] <- 0

# Subtrair matriz superior da inferior

insumo_produto_20_liquido <- insumo_produto_20_superior - t(insumo_produto_20_inferior)

# Formatar matriz para "pair-list" 
# Inverter ordem do par quando valor negativo

ligacoes <- as.data.frame(as.table(insumo_produto_20_liquido)) %>%
  filter(Freq != 0) %>%
  mutate(label_from = as.character(case_when(Freq < 0 ~ Var2,
                                             Freq > 0 ~ Var1,
                                             TRUE ~ Var1)),
         label_to = as.character(case_when(Freq < 0 ~ Var1,
                                           Freq > 0 ~ Var2,
                                           TRUE ~ Var2))) %>%
  mutate(valor = abs(Freq)) %>%
  select(label_from, label_to, valor) %>%
  inner_join(cod_produtos, by = c("label_from" = "label")) %>%
  inner_join(cod_produtos, by = c("label_to" = "label")) %>%
  select(from = id.x, to = id.y, valor)



# Gerar visualização no visnetwork

# Atualizar cod_produtos

cor <- ligacoes %>%
  group_by(to) %>%
  summarise(value = sum(valor)) %>%
  mutate(color = colorize(value,colors = c("#FFFFFF", "5E0010", "#4b000c")))

cod_produtos_novo <- cod_produtos %>%
  left_join(cor, by = c("id" = "to")) %>%
  filter(id %in% c(ligacoes$from, ligacoes$to))

ligacoes_novo <- ligacoes %>%
  group_by(from) %>%
  slice_max(order_by = valor, n = 1) %>% # Selecionada a ligação mais forte para cada insumo
  mutate(arrows = "to")

set.seed(1234)
visNetwork(cod_produtos_novo, ligacoes_novo, height = "900px", width = "100%")


