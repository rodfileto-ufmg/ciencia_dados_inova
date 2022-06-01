library(tidyverse)
library(ggplot2)
library(highcharter)
library(visNetwork)
library(textstem)
library(tidytext)
library(igraph)

text_data_files <- list.files("./06_data_viz/dados/text_data", full.names = TRUE)

general_data_files <- list.files("./06_data_viz/dados/general_data", full.names = TRUE)

general_data <- map_dfr(general_data_files, read_csv) %>% separate_rows(subject, sep = ";")

general_data_year <- general_data %>%
  filter(!(is.na(subject))) %>%
  group_by(scielo_id) %>%
  mutate(frac = 1/n()) %>%
  group_by(year_published_scielo, subject) %>%
  summarise(frac = sum(frac))

text_data <- map_dfr(text_data_files, read_csv)


# line plot by year and field

ggplot(general_data_year, mapping = aes(x = year_published_scielo, y = frac, colour = subject)) +
  geom_line() + labs(x = "", y = "", colour = "Campo científico") + theme_classic() + 
  theme(legend.position = "bottom")

ggplot(general_data_year, mapping = aes(x = year_published_scielo, y = frac)) +
  geom_line() + labs(x = "", y = "", colour = "Campo científico") + theme_classic() + 
  facet_wrap(~ subject)

hchart(general_data_year, "line", hcaes(x = year_published_scielo, y = frac, group = subject)) %>%
  hc_yAxis(labels = list(style = list(color = "#525151",
                                                           font= '15px Helvetica',
                                                           fontWeight = "bold")), title = list(text = "")) %>%
  hc_xAxis(title = list(text = ""), labels = list(style = list(color = "#525151",
                                                               font= '15px Helvetica',
                                                               fontWeight = "bold"
  )))


# É possível também fazer painéis no highcharter. Ver https://stackoverflow.com/questions/53277199/facet-wrap-facet-grid-any-similar-function-in-highcharter

# Bar chart - keywords por campo científico

keyword <- text_data %>%
  filter(feature == "keyword" & language == "en") %>%
  separate_rows(text, sep = ";") %>%
  mutate(text = str_squish(text)) %>%
  mutate(text_lemma = tolower(lemmatize_strings(text))) %>%
  filter(nchar(text_lemma) >= 3) %>%
  filter(!(grepl("&", text_lemma, fixed = TRUE))) 


keyword %>%
  inner_join(general_data, by = c("id" = "scielo_id")) %>%
  count(subject, text_lemma, sort = TRUE) %>%
  group_by(subject) %>%
  slice_max(order_by = n, n = 5) %>%
  ungroup() %>%
  # mutate(text_lemma = fct_reorder(text_lemma, n)) %>%
  ungroup() %>%
  ggplot(aes(n,  reorder_within(text_lemma, n, subject), fill = subject)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() + 
  facet_wrap(~subject, ncol = 2, scales = "free") +
  labs(x = "Frequência", y = NULL)

# Rede de palavras

keyword_network <- keyword %>%
  inner_join(keyword, by = "id") %>%
  filter(text_lemma.x < text_lemma.y) %>%
  group_by(text_lemma.x, text_lemma.y) %>%
  summarise(n = n()) %>%
  filter(n > 10)

# Igraph

colors <- data.frame(subject = c("Health Sciences", "Biological Sciences", "Human Sciences",
                               "Applied Social Sciences", "Agricultural Sciences",
                               "Engineering", "Linguistics, Letters and Arts",
                               "Exact and Earth Sciences"), 
                     color_science = c("#19D400","#F5A511","#8F00B3","#0000FF","#BD0016", "#ABEBC6",  "#EDBB99", "#F1948A"))

vertices_subject <- keyword %>%
  inner_join(general_data, by = c("id" = "scielo_id")) %>%
  count(subject, text_lemma, sort = TRUE) %>%
  group_by(text_lemma) %>%
  slice_max(order_by = n, n = 1) %>%
  select(-n)

vertices <- keyword %>%
  count(text_lemma, sort = TRUE) %>%
  filter(text_lemma %in% c(keyword_network$text_lemma.x, keyword_network$text_lemma.y)) %>%
  inner_join(vertices_subject) %>%
  distinct(text_lemma, .keep_all = TRUE) %>%
  inner_join(colors)

unique(vertices$subject)

keyword_network <- keyword_network %>%
  filter(text_lemma.x %in% vertices$text_lemma & text_lemma.y %in% vertices$text_lemma)
  

graph <- graph_from_data_frame(keyword_network, vertices = vertices, directed = FALSE)

# E(net_doc)$lty <- 0

# E(net_doc)$color <- "#FF000000"

l <- layout_with_drl(graph)

# png("./intelligence innovation/Output/shiny/RF/www/doc_net.png", width = 20, height = 20, res = 500, units = "in", bg="transparent")
plot(graph, layout = l, vertex.color = adjustcolor(V(graph)$color_science, alpha.f = .8), vertex.size = V(graph)$n/100, vertex.frame.color = "#FF000000")


# Visnetwork

nodes <- vertices %>%
  mutate(id = 1:nrow(.)) %>%
  mutate(title = glue::glue("<tr><td>Número de documentos:</td><td>{n}</td></tr>")) %>%
  select(id, title, label = text_lemma, value = n, group = subject, color = color_science)

edges <- keyword_network %>%
  left_join(select(nodes, id, label), by = c("text_lemma.x" = "label")) %>%
  left_join(select(nodes, id, label), by = c("text_lemma.y" = "label")) %>%
  mutate(width = n/10) %>%
  select(from = id.x, to = id.y, width)

visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(selectedBy = "group")




