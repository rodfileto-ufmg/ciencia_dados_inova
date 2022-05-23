library(bib2df)
library(tidyverse)

files <- list.files(pattern = "references.bib", full.names = TRUE, recursive = TRUE)

bib_files <- map_dfr(files, bib2df)


