library(tidyverse)
library(igraph)
library(rvest)

all_countries <- countrycode::countryname_dict %>% select(country.name.en) %>% distinct() %>% pull(country.name.en) 

main_article <- read_html("https://en.wikipedia.org/wiki/List_of_sister_cities_in_Europe")
sistertowns <- main_article %>% html_nodes("a") %>% map_df(~{
  href = .x %>% html_attr("href")
  title = .x %>% html_attr("title")
  tibble(title, href)
}) 

sistertowns <- sistertowns %>% 
  filter(title %>% str_detect("List of")) %>% 
  mutate(href = paste("https://en.wikipedia.org/", href, sep = ""),
         country = str_replace_all(title, "List of twin towns and sister cities in ", "")) %>% select(country, href) %>% head(5)

get_twintowns <- function(x){
  x <- sistertowns[[1,2]]
  sister_incountry <- x %>% read_html() %>% html_nodes("p") %>% html_text() %>% str_remove_all("\\W*") %>% str_remove_all("\\d")
  aux <- x %>% read_html() %>% html_nodes("p, li") %>% html_text()
  aux %>% head(20)
  for(i in 1:length(aux)){
    aux[[i]] %in% sister_incountry
  }
  return(list(aux))
}

sistertowns %>% rowwise() %>% mutate(sisters = get_twintowns(href)) %>% unnest(sisters)
sistertowns[[1,2]] %>% read_html() %>% html_nodes("p") %>% html_nodes("b") %>% html_nodes("a") %>% html_attr("title")
sistertowns[[1,2]] %>% read_html() %>% html_nodes("p, li")
get_twintowns(sistertowns[[2,2]]) 

sistertowns[[2,2]] %>% read_html() %>% html_nodes("div[id = 'mw-content-text']")  %>% 
aux1 <-aux[[6]]
aux[[17]] %>% html_text()
aux %>% html_text() %>%
main_article %>% html_nodes("a") %>% html_attr("href")
main_article %>% html_nodes("a") %>% html_attr("title")
  

