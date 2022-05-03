library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(fuzzyjoin)
library(phonics)
library(stringdist)


##Two problems
#Merge cited references with the documents in the corpus
#merge cited references that are the same

core_CR <- readr::read_csv("./03_cienciometria/dados/core.csv") %>% 
  select(UT, CR) %>%
  separate_rows(., CR, sep = "; ") %>%
  filter(!(is.na(CR) | CR == ""))

# 
system.time({
core_CR <- core_CR %>%
  # filter(row_number() <= 1000) %>%
  filter(!(is.na(CR) | CR == "")) %>%
  transform(CR = strsplit(CR, ".;")) %>%
  unnest(CR) %>%
  mutate(CR = gsub("<.*?>", "", CR)) %>% # deleting html symbols
  # mutate(CR_edited = CR) %>%
  mutate(AU = trimws(as.character(lapply(strsplit(CR, ","), "[", 1)))) %>%
  mutate(PY = trimws(as.character(lapply(strsplit(CR, ","), "[", 2)))) %>%
  mutate(JI = trimws(as.character(lapply(strsplit(CR, ","), "[", 3)))) %>%
  mutate(VL = trimws(as.character(lapply(strsplit(CR, ","), "[", 4)))) %>%
  mutate(PG = trimws(as.character(lapply(strsplit(CR, ","), "[", 5)))) %>%
  filter(!(grepl("ANONYMOUS", ignore.case = TRUE, AU))) %>%
  mutate(PY = gsub("[^0-9 ]", "", PY)) %>% #Keep only the 4 numbers in Pub year
  mutate(PY = gsub(" ", "", PY, fixed = TRUE)) %>%
  mutate(PY = ifelse(nchar(PY) == 4, PY, "")) %>%
  mutate(PY_l = as.numeric(PY) - 1) %>%
  mutate(PY_p = as.numeric(PY) + 1) %>%
  # filter(PY >= 2008) %>%
  mutate_all(as.character) %>%
  mutate(DI = sub(".*,", "", CR)) %>%
  mutate(DI = ifelse(grepl("DOI ", DI), DI, "NA")) %>%
  mutate(DI = gsub("\\]", "", DI, perl=T)) %>%
  mutate(DI = gsub("DOI ", "", DI)) %>%
  mutate(DI = gsub("\\[", "", DI)) %>%
  mutate(DI = sub(".*,", "", DI)) %>%
  mutate(DI = gsub("\\.$", "", DI)) %>%
  mutate(DI = trimws(DI)) %>%
  mutate(AU = as.character(lapply(strsplit(AU, " "), "[", 1))) %>%
  # mutate(AU = trimws(gsub(" ", "", AU, fixed = TRUE))) %>%
  mutate(au_soundex = refinedSoundex(AU, maxCodeLen = 10L)) %>%
  mutate(JI = gsub(" ", "", JI, fixed = TRUE)) %>%
  mutate(VL = trimws(gsub("[^0-9 ]", "", VL))) %>% #Keep only numbers in vol
  mutate(PG = ifelse(grepl("DOI", PG), "NA", PG)) %>%
  mutate(PG = trimws(gsub("[^0-9 ]", "", PG))) %>% #Keep only numbers in page
  mutate_all(as.character) %>%
  mutate_all(str_squish)
})


# readr::write_csv(core_CR, "./data/core_CR.csv")


# core_CR <- readr::read_csv("./data/core_CR.csv") %>%
  # mutate_all(as.character)

articles_map <- readr::read_csv("./03_cienciometria/dados/core.csv") %>%
  select(UT, JI, PY, AU, VL, PG, DI) %>%
  mutate_all(as.character) %>%
  mutate(AU = trimws(as.character(lapply(strsplit(AU, ";"), "[", 1)))) %>%
  mutate(PY = as.numeric(PY)) %>%
  # mutate(SO = trimws(as.character(lapply(strsplit(SR, ","), "[", 3)))) %>%
  mutate(AU = gsub("[^A-Za-z ]", "", AU)) %>% # Author name without numbers or symbols
  mutate(AU = as.character(lapply(strsplit(AU, " "), "[", 1))) %>% # Author last name
  mutate(AU = gsub(" ", "", AU, fixed = TRUE)) %>% # Author name without spaces
  mutate(au_soundex = refinedSoundex(AU, maxCodeLen = 10L)) %>%
  mutate(JI = gsub("[^A-Za-z ]", "", JI)) %>%
  mutate(JI = gsub(" ", "", JI, fixed = TRUE)) %>%
  mutate(DI = gsub(" ", "", DI, fixed = TRUE)) %>%
  mutate(PY_l = as.numeric(PY) - 1) %>%
  mutate(PY_p = as.numeric(PY) + 1) %>%
  mutate(VL = gsub("[^0-9 ]", "", VL)) %>%
  mutate(PG = gsub(" ", "", PG, fixed = TRUE)) %>%
  mutate(PG = sub("-.*", "", PG)) %>%
  mutate(PG = gsub("[^0-9 ]", "", PG)) %>%
  mutate_all(str_squish)

columns_1 <- c("AU", "PY", "JI", "VL", "PG", "DI")
columns_2 <- c("AU", "PY", "JI", "VL", "PG")
columns_3 <- c("AU", "PY", "JI", "VL", "DI")
columns_4 <- c("AU", "PY", "JI", "PG", "DI")
columns_5 <- c("AU", "PY", "JI", "DI")
columns_6 <- c("AU", "PY", "DI")
columns_7 <- c("au_soundex", "PY", "JI", "VL", "PG", "DI")
columns_7 <- c("au_soundex", "PY", "JI", "VL", "PG")
columns_8 <- c("au_soundex", "PY", "JI", "VL", "DI")
columns_9 <- c("DI")


ref_match <- function(columns,ordering,match_number) {

  columns_filter <- paste0(paste0("(",columns," == 'NA')"), collapse = " | ")

  columns_filter <- paste0("!(", columns_filter, ")")

  match <- core_CR %>%
    filter_(columns_filter) %>%
    # filter(CR %in% !!(paste0("match_",match_number))$CR) %>%
    inner_join(articles_map, by = columns, suffix=c("_x", "_y")) %>%
    mutate(match_order = ordering) %>%
    select(UT_x, UT_y, match_order, CR)
}


match_1 <- ref_match(columns_1, 1)
match_2 <- ref_match(columns_2, 2)
match_3 <- ref_match(columns_3, 3)
match_4 <- ref_match(columns_4, 4)
match_5 <- ref_match(columns_5, 5)
match_6 <- ref_match(columns_6, 6)
match_7 <- ref_match(columns_7, 7)
match_8 <- ref_match(columns_8, 8)
match_9 <- ref_match(columns_9, 9)

my.list <- mget(ls(pattern="^match_*"))

core <- select(readr::read_csv("./03_cienciometria/dados/core.csv"), UT,PY) %>% mutate_all(as.character)

# 
no_match <- core_CR %>%
  filter(!(CR %in% c(match_1$CR, match_2$CR, match_3$CR, match_4$CR, match_5$CR, match_6$CR, match_7$CR, match_8$CR, match_9$CR))) %>%
  filter(!(is.na(PY) | PY %in% c("", " ", "   "))) %>%
  mutate(UT_y = paste(AU, PY, JI, VL, PG, DI)) %>%
  mutate(UT_y = gsub('NA', '', UT_y)) %>%
  mutate_all(str_squish) %>%
  select(UT_x = UT, UT_y, PY_y = PY, DI) %>%
  mutate(UT_y = ifelse(is.na(DI) | DI == "NA", UT_y, DI)) %>%
  left_join(core, by = c("UT_x" = "UT"), suffix=c("_y", "_x")) %>%
  select(UT_x, UT_y, PY_x = PY, PY_y)

match_total <- bind_rows(my.list) %>%
  distinct(UT_x, UT_y) %>%
  left_join(core, by = c("UT_x" = "UT"), suffix=c("_x", "_y")) %>%
  left_join(core, by = c("UT_y" = "UT"), suffix=c("_x", "_y")) %>%
  bind_rows(no_match)


write_csv(match_total, "./03_cienciometria/dados/citing_cited.csv")

