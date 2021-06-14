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
         country = str_replace_all(title, "List of twin towns and sister cities in ", "")) %>% select(country, href)

get_twintowns <- function(x){
  x %>% read_html() %>% html_nodes("p") %>% html_nodes("b") %>% html_node("a") %>% html_attr("title")
}

sistertowns %>% group_by(country) %>% mutate(twin)
sistertowns[[1,2]] %>% read_html() %>% html_nodes("p") %>% html_nodes("b") %>% html_node("a") %>% html_attr("title")
aux <- sistertowns[[1,2]] %>% read_html() %>% html_nodes("li")
aux[[17]] %>% html_text()
aux %>% html_text() %>%
main_article %>% html_nodes("a") %>% html_attr("href")
main_article %>% html_nodes("a") %>% html_attr("title")
  

