library(rvest)
library(tidyverse)
library(RCurl)
library(furrr)



page <- read_html("https://analytics.scielo.org/w/reports")


zip_links <- page %>% html_nodes("ul > li > a") %>% html_attr("href") %>%
  as_tibble() %>%
  filter(grepl("*.zip", value)) %>%
  mutate(url = sub("https", "http", fixed = TRUE, value))


dir.create("./03_cienciometria/dados/scielo")

plan(multisession, workers = 7)

future_map(as.vector(zip_links$url)[3], ~download.file(.x, destfile = paste0("./03_cienciometria/dados/scielo/", basename(.x), collapse = "")))


