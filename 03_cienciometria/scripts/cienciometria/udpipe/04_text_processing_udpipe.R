library(tidyverse)
library(udpipe)
library(stringi)


text_data <- read_csv("./03_cienciometria/dados/core.csv") %>%
  mutate(text = tolower(glue::glue("{TI} {AB}"))) %>%
  select(UT, text)


if (!(file.exists("english-ewt-ud-2.5-191206.udpipe"))) {
  
  en <- udpipe_download_model(language = "english")
  
}

udmodel_en <- udpipe_load_model(file = ifelse(file.exists("english-ewt-ud-2.5-191206.udpipe"),"english-ewt-ud-2.5-191206.udpipe",
                                              en$file_model))



x <- udpipe_annotate(udmodel_en, x = text_data$text, doc_id = text_data$UT,  tagger = "default", parser = "default") %>%
  as_tibble()



nounprhases <- x %>%
  mutate(pos_abbr = case_when(upos %in% c("NOUN", "PROPN") ~ 'n', 
                              upos == "ADJ" ~ 'a',
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
  count(doc_id, noun_phrase) %>%
  filter(nchar(noun_phrase) > 3) %>%
  rename(UT = doc_id)



write_csv(nounprhases, "./03_cienciometria/dados/noun_phrases.csv")
