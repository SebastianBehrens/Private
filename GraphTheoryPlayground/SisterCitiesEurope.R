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
  # x <- sistertowns[[2,2]]
  # getting all in-country towns that are connected to other towns
  sister_incountry <- x %>% read_html() %>% html_nodes("p") %>% html_text() %>% str_remove_all("[\\[\\]\\d]") %>% str_trim()
  sister_incountry <- sister_incountry[(str_length(sister_incountry) %>% between(1, 30))]
  
  aux <- x %>% read_html() %>% html_nodes("p, li") %>% html_text() %>% str_trim() %>% str_remove_all("[\\[\\]\\d]")
  positions <- which(aux %in% sister_incountry)
  begin_footnotes <- which(aux %>% str_detect("\\^")) %>% min() # final sister town right before the first footnote
  
  out <- tibble(sisterx = character(), sistery = character())
  for(i in 1:length(positions)){
    # i <- 1
    start_position <- positions[i] + 1
    end_position <- positions[i+1]-1
    end_position <- ifelse(is.na(end_position), begin_footnotes - 1, end_position)
    out <- out %>% complete(sisterx = aux[[positions[i]]], sistery = aux[start_position:end_position])
  }
  return(list(out))
}


sistertowns <- sistertowns %>% rowwise() %>% mutate(sisters = get_twintowns(href)) 
sistertowns <-
  sistertowns %>% 
  unnest(sisters) %>% 
  rename(country.x = country, sister.x = sisterx) %>% 
  separate(sistery, c("sistertown.y", "countr.y"), ",\\s") %>% 
  select(sister.x, country.x, sister.y, country.y)

testing <- sistertowns %>% 
  unnest(sisters) %>% 
  rename(country.x = country, sister.x = sisterx)
testing[c(152, 207, 338, 444, 451, 547, 604, 718, 818, 1031, 1114, 1272, 1296, 1336, 1373, 1377, 1389, 1411, 1448, 1460),]
