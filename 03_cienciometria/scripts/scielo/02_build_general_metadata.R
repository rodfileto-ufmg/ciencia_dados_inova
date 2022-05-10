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


#### Documents Authors
 
documents_author <- future_map_dfr(.x = zip_files[3], ~read_csv(unz(.x, "documents_authors.csv")) %>%
                                     mutate_all(as.character))


documents_author %>% 
  distinct(`document publishing ID (PID SciELO)`, `document author`, .keep_all = TRUE) %>%
  count(`document author affiliation country`, sort = TRUE)
  

#### documents dates #####

documents_dates <- future_map_dfr(.x = zip_files[3], ~read_csv(unz(.x, "documents_dates.csv")) %>%
                                    mutate_all(as.character))





