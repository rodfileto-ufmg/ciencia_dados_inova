library(tidyverse)
library(furrr)

plan(multisession, workers = 7)

# load zip files
zip_files <- list.files(path = "./03_cienciometria/dados/scielo/", full.names = TRUE, pattern = "*.zip")

#### Affilliation


documents_affiliations <- future_map_dfr(.x = zip_files[3], ~read_csv(unz(.x, "documents_affiliations.csv")) %>%
                                           mutate_all(as.character))
  
documents_affiliations <- documents_affiliations %>%
  select(scielo_id = `document publishing ID (PID SciELO)`,
         organization = `document affiliation instituition`,
         country = `document affiliation country`,
         country_code = `document affiliation country ISO 3166`,
         state = `document affiliation state`,
         city = `document affiliation city`)


#### Languages

documents_languages <- future_map_dfr(.x = zip_files[3], ~read_csv(unz(.x, "documents_languages.csv")) %>%
                                           mutate_all(as.character))

# documents_languages <- documents_languages %>%
#   select(scielo_id = `document publishing ID (PID SciELO)`,
#          language = `document languages`)


#### Document Author
 
documents_author <- future_map_dfr(.x = zip_files[3], ~read_csv(unz(.x, "documents_authors.csv")) %>%
                                     mutate_all(as.character))


documents_author %>% 
  distinct(`document publishing ID (PID SciELO)`, `document author`, .keep_all = TRUE) %>%
  count(`document author affiliation country`, sort = TRUE)
  

#### General metadata #####

# Scielo ID
# DOI
# Journal

documents_dates <- foreach(file = as.vector(list.files(path = "./zip_data", full.names = TRUE)), .combine = bind_rows) %dopar% {
  
  documents_altmetrics <- read_csv(unz(file, "documents_dates.csv")) %>%
    mutate_all(as.character)
  
}

general_data <- documents_dates %>%
  select(scielo_id = `document publishing ID (PID SciELO)`,
         ISSN_scielo = `ISSN SciELO`,
         ISSN = `ISSN's`,
         journal = `title at SciELO`,
         type = `document type`,
         year_submitted = `document submitted at year`,
         year_accepted = `document accepted at year`,
         year_published = `document published at year`,
         year_published_scielo = `document published in SciELO at year`,
         subject = `title thematic areas`
         ) %>%
  distinct()




