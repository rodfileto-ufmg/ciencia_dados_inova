library(tidyverse)
library(igraph)
library(sparklyr)
library(tidyverse)
library(purrr)
library(glue)

resolution_parameter = 0.002

min_cluster_size = 5

citing_cited <- read_csv("./03_cienciometria/dados/citing_cited.csv")

#### Defining the links between documents ####

# Example below: bibliographic coupling

bib_coupling <- citing_cited %>%
  inner_join(select(citing_cited, UT_x, UT_y), by = "UT_y") %>%
  filter(!(UT_x.x == UT_x.y)) %>%
  group_by(UT_x.x, UT_x.y) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  group_by(UT_x.x) %>%
  slice_max(order_by = weight, n = 20) %>%
  mutate(weight = weight/sum(weight)) %>%
  ungroup()

core <- read_csv("./03_cienciometria/dados/core.csv") %>%
  select(UT, TI, SO, PY, SC, WC) %>%
  filter(UT %in% c(bib_coupling$UT_x.x, bib_coupling$UT_x.y))

#### Mapa de documentos ####

colors <- data.frame(group = c("Biomedical and health sciences", "Life and earth sciences", "Mathematics and computer science",
                               "Physical sciences and engineering", "Social sciences and humanities"), color_science = c("#19D400","#F5A511",
                                                                                                                         "#8F00B3",
                                                                                                                         "#0000FF","#BD0016"))

map_doc <- core %>%
  separate_rows(WC, sep = ";") %>%
  mutate(WC = str_squish(tolower(WC))) %>% 
  mutate(WC = gsub("\\&", "&", WC, fixed = TRUE)) %>%
  left_join(fields, by = c("WC" = "Journal subject category")) %>%
  mutate(id = paste0(UT,"_", Field), value = 1) %>% rename(group = Field) %>% left_join(colors) %>%
  distinct(id, .keep_all = TRUE)


network_doc <- bib_coupling %>%
  filter(UT_x.x %in% core$UT & UT_x.y %in% core$UT) %>% 
  left_join(map_doc, by = c("UT_x.x" = "UT")) %>%
  left_join(map_doc, by = c("UT_x.y" = "UT")) %>% 
  select(from = id.x, to = id.y, weight)


map_doc <- map_doc %>%
  distinct(id, group, color_science) %>%
  mutate(group = case_when(is.na(group) ~ "Multidisciplinary",
                           TRUE ~ as.character(group)),
         color_science = case_when(is.na(color_science) ~ "#808080",
                                   TRUE ~ as.character(color_science))) %>%
  select(id, group, color_science)


net_doc <- graph_from_data_frame(network_doc, vertices = map_doc)

E(net_doc)$lty <- 0

E(net_doc)$color <- "#FF000000"

l <- layout_with_drl(net_doc)

# png("./intelligence innovation/Output/shiny/RF/www/doc_net.png", width = 20, height = 20, res = 500, units = "in", bg="transparent")
plot(net_doc, layout = l, vertex.label = NA,
     vertex.color = adjustcolor(V(net_doc)$color_science, alpha.f = .8), vertex.size = 4, vertex.frame.color = "#FF000000")
# dev.off()


#### Clustering Process ####

graph <- graph_from_data_frame(bib_coupling, vertices = core, directed = FALSE)

##### Filter only the graph big component ####
components <- igraph::clusters(graph)

biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(graph)[components$membership == biggest_cluster_id]

# subgraph
graph <- induced_subgraph(graph, vert_ids)

##### Clustering algorithm ####

RF <- cluster_leiden(graph, resolution_parameter = resolution_parameter)

clusters <- tibble(cluster = RF$membership, UT = RF$names)

#### Filter 

new_core <- core %>%
  inner_join(clusters) %>%
  group_by(cluster) %>%
  filter(n() >= min_cluster_size) %>%
  ungroup()

nrow(new_core)/nrow(core)

cluster_size <- new_core %>%
  count(cluster, sort = TRUE)

hist(cluster_size$n)

##### cluster similarity based on bibliographic coupling ####

# cluster_similarity_bib_coupling <- citing_cited %>%
#   inner_join(select(citing_cited, UT_x, UT_y), by = "UT_y") %>%
#   filter(!(UT_x.x == UT_x.y)) %>%
#   inner_join(select(new_core, UT, cluster), by = c("UT_x.x" = "UT")) %>%
#   inner_join(select(new_core, UT, cluster), by = c("UT_x.y" = "UT")) %>%
#   filter(cluster.x != cluster.y) %>%
#   group_by(cluster.x, cluster.y) %>%
#   summarise(weight = n()) %>%
#   ungroup() %>%
#   group_by(cluster.x) %>%
#   slice_max(order_by = weight, n = 1) %>%
#   mutate(weight = weight/sum(weight)) %>%
#   ungroup()
  
  


##### cluster similarity based on textual distance ####

nounphrases <- read_csv("./03_cienciometria/dados/noun_phrases.csv") %>%
  inner_join(new_core) %>%
  filter(nchar(noun_phrase) > 2) %>%
  count(cluster, noun_phrase)

document_length <- nounphrases %>%
  group_by(cluster) %>%
  summarise(n = sum(n)) %>%
  filter(n > 10) %>%
  ungroup() %>%
  mutate(avg_doc_length = mean(n)) %>%
  rename(d = n)

total_documents <- nrow(distinct(new_core, cluster))


idf <- nounphrases %>%
  group_by(noun_phrase) %>%
  summarise(q = n_distinct(cluster)) %>%
  ungroup() %>%
  filter(q >= 5) %>%
  mutate(idf_m = log((total_documents - q + 0.5)/(q + 0.5)) + 1)

bm25 <- nounphrases %>%
  group_by(cluster, noun_phrase) %>%
  ungroup() %>%
  left_join(document_length) %>%
  inner_join(idf, by = "noun_phrase") %>%
  mutate(bm25 = idf_m * ((n * (2 + 1)) / (n + 2 * (1- 0.75 + 0.75 * (d/avg_doc_length))))) %>%
  select(cluster, noun_phrase, bm25) %>% 
  filter(!(noun_phrase %in% c("article", "paper", "objective", "year", "data", "conclusion", "work", "abstract", "research", "analysis"))) %>%
  arrange(cluster, noun_phrase)

cluster_similarity <- bm25 %>%
  inner_join(bm25, by = "noun_phrase") %>%
  filter(cluster.x != cluster.y) %>%
  mutate(bm25 = (bm25.x + bm25.y)/2) %>%
  group_by(cluster.x, cluster.y) %>%
  summarise(bm25 = sum(bm25)) %>%
  ungroup() %>%
  group_by(cluster.x) %>%
  mutate(bm25 = bm25/sum(bm25)) %>%
  slice_max(order_by = bm25, n = 20) %>%
  ungroup() %>%
  filter(bm25 > 0) %>%
  rename(weight = bm25)


##### Clustering caracteristics ####

avg_year <- new_core %>%
  filter(!(is.na(PY))) %>%
  group_by(cluster) %>%
  summarise(avg_year = mean(PY))

fields <- readxl::read_xlsx("./03_cienciometria/dados/main_field.xlsx") %>%
  mutate(`Journal subject category` = tolower(`Journal subject category`)) %>%
  mutate_all(str_squish)

science <- new_core %>%
  separate_rows(WC, sep = ";") %>%
  mutate(WC = str_squish(tolower(WC))) %>% 
  mutate(WC = gsub("\\&", "&", WC, fixed = TRUE)) %>%
  left_join(fields, by = c("WC" = "Journal subject category")) %>%
  group_by(UT) %>%
  mutate(weight = 1/n()) %>%
  group_by(cluster, Field) %>%
  summarise(weight = sum(weight)) %>%
  group_by(cluster) %>%
  slice_max(order_by = weight, n = 1) %>%
  select(-weight) %>%
  ungroup()


category <- new_core %>%
  separate_rows(WC, sep = ";") %>%
  mutate(WC = str_squish(tolower(WC))) %>% 
  mutate(WC = gsub("\\&", "&", WC, fixed = TRUE)) %>%
  left_join(fields, by = c("WC" = "Journal subject category")) %>%
  group_by(UT, WC, cluster) %>%
  mutate(weight = 1/n()) %>%
  ungroup() %>%
  group_by(cluster, WC) %>%
  summarise(weight = sum(weight)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  slice_max(order_by = weight, n = 10) %>% 
  aggregate(WC ~ cluster, ., paste, collapse = "; " ) %>%
  mutate(journal_category = paste0("<tr><td>Most frequent categories:</td><td>", WC,"</td></tr>")) %>%
  select(-WC)

label <- bm25 %>%
  group_by(cluster) %>%
  slice_max(order_by = bm25, n = 1) %>%
  select(-bm25) %>%
  rename(label = noun_phrase)

keywords <- bm25 %>%
  group_by(cluster) %>%
  slice_max(order_by = bm25, n = 10) %>%
  aggregate(noun_phrase ~ cluster, ., paste, collapse = "; " ) %>%
  mutate(noun_phrase = paste0("<tr><td>Most frequent terms:</td><td>", noun_phrase,"</td></tr>"))

# 
# map_clusters <- science %>%
#   mutate(description = paste0("<table>", Field))

map_clusters <- new_core %>%
  count(cluster) %>%
  left_join(category) %>%
  left_join(science) %>%
  left_join(label) %>%
  left_join(keywords) %>%
  left_join(avg_year) %>%
  rename(id = cluster, weight = n) %>%
  mutate(description = paste0("<table>", journal_category, noun_phrase, "</table>")) %>%
  select(-journal_category, -Field, -noun_phrase, "score<avg-year>" = avg_year) %>%
  distinct(id, .keep_all = TRUE)

write.table(map_clusters, "./03_cienciometria/dados/map_clusters.txt", sep = ";", quote = TRUE, row.names = FALSE, col.names = TRUE)

write.table(cluster_similarity, "./03_cienciometria/dados/network_clusters.txt", sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

rstudioapi::terminalExecute(glue::glue("java -jar VOSviewer.jar -map {normalizePath(getwd())}//03_cienciometria/dados/map_clusters.txt -network {normalizePath(getwd())}/03_cienciometria/dados/network_clusters.txt -largest_component -attraction 4 -repulsion -1"))


