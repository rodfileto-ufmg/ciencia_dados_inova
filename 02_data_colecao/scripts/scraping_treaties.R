library(rvest)
library(tidyverse)
library(purrr)

get_metada_year <- function(y) {
  
  url_year <- paste("https://www.state.gov/",y,"-TIAS/",sep = "")
  
  url <- read_html(url_year)
  
  n_elements <- url %>%
    html_node(".collection-info__total") %>%
    html_node(".collection-info__number") %>%
    html_text() %>%
    as.numeric()
  
  n_elements <- ceiling(n_elements/10)


  get_metadata <- function(i) {
    
    url_temp <- if(i == 1) {
      
      url
      
    } else {read_html(paste(url_year,"page/", i,"/", sep = ""))}
    
    
    
    metadata <- url_temp %>% # Extract metadata from the main page
      html_nodes(".collection-result__link") %>%
      html_text() %>%
      as_tibble() %>%
      mutate(treaty_number = gsub(".*\\((.*)\\).*", "\\1", value)) %>% # Extract text between parenthesis
      mutate(country = sub("\\(.*", "", value)) %>%
      mutate(title = sub(".*–", "", value))
    
    pub_date <- url_temp %>% 
      html_nodes(".collection-result-meta") %>%
      html_text()
    
    links <- url_temp %>% 
      html_nodes(".collection-result__link") %>%
      html_attr("href")
    
    metadata <- metadata %>%
      cbind(pub_date, links)
    
    
    
    get_content <- function(x) {
      
      content <- read_html(x) %>%
        html_nodes(".row") %>%
        html_nodes(".entry-content") %>%
        html_children()
      
      content_link <- content %>% html_attr("href") %>%
        na.omit(.)
      
      
      content <- tibble(category = content[[1]] %>% html_text(), description = content[[2]] %>% html_text(), download_link = content_link)  
      
    }
    
    content <- map(as.vector(metadata$links), get_content) %>%
      bind_rows()
    
    metadata <- cbind(metadata, content)
    
    return(metadata)
    
  }
  
  metadata <- map(1:n_elements, get_metadata) %>%
    bind_rows()
  
  return(metadata)
  
}

metadata <- map(2008, get_metada_year) %>%
  bind_rows()

dir.create("./02_data_colecao/dados")

map(as.vector(metadata$download_link[1]), ~download.file(.x, destfile = paste("./02_data_colecao/dados/", basename(.x), sep ="")))





# 
# html_nodes(pg, ".zone") %>% 
#   map_df(~{
#     data_frame(
#       postal = html_node(.x, "span") %>% html_text(trim=TRUE),
#       city = html_nodes(.x, "ul > li") %>% html_text(trim=TRUE)
#     )
#   }) 
