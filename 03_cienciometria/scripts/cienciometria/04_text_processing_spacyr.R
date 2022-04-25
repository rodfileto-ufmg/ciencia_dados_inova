library(spacyr)
library(tidyverse)
library(stringi)
library(furrr)


# spacy_install()

# spacy_download_langmodel("en_core_web_lg")

spacy_initialize(model = "en_core_web_lg")

text_data <- read_csv("./03_cienciometria/dados/core.csv") %>%
  mutate(text = glue::glue("{TI} {AB}")) %>%
  select(UT, text)
  
  
n <- 100

text_data_splited <- split(text_data, rep_len(1:n, nrow(text_data)))




extract_terms <- function(x) {


corpus <- x %>%
  select(doc_id = UT, text) %>%
  mutate(text = str_squish(gsub("&#8220;|&#8221;|# 956;|&#37|# 956;m|&#8217", "", text)))
  
  

parsedtxt <- spacy_parse(corpus, nounphrase = TRUE, multithread = TRUE) %>%
  filter(!(grepl("=|%", token))) %>%
  filter(token != "-")


# https://github.com/quanteda/spacyr/issues/190
# 

nounprhases <- parsedtxt %>%
  mutate(pos_abbr = case_when(pos == "NOUN" ~ 'n', 
                              pos == "ADJ" ~ 'a',
                              TRUE ~ 'v')) %>% # create an abbr variable
  group_by(doc_id, sentence_id) %>%
  summarize(pos_seq = paste(pos_abbr, collapse = ""), # concatenate abbr variable
            txt = list(lemma)) %>% # keep the text as a string vector
  mutate(pos2 = stri_locate_all_regex(pos_seq, "a?(n|a)*n+")) %>% # find the matching
  mutate(start_pos = map(pos2, ~.[, 1])) %>% 
  mutate(end_pos = map(pos2, ~.[, 2])) %>%
  unnest(c(start_pos, end_pos)) %>%
  filter(!(is.na(start_pos) | is.na(end_pos))) %>%
  rowwise() %>%
  mutate(noun_phrase = paste(txt[start_pos:end_pos], collapse = " ")) %>%
  ungroup()  %>%
  count(doc_id, noun_phrase)

}

noun_phrases <- future_map(text_data_splited, extract_terms, .options = furrr_options(seed = 123)) %>%
  bind_rows() %>%
  group_by(doc_id, noun_phrase) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  rename(UT = doc_id)

write_csv(noun_phrases, "./03_cienciometria/dados/noun_phrases.csv")

spacy_finalize()

