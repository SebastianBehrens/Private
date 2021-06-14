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
sistertowns <- sistertowns %>% filter(country != "North Cyprus")
sistertowns <- sistertowns[c(1:45),]

get_twintowns <- function(x){
  # x <- sistertowns[[1,2]]
  # getting all in-country towns that are connected to other towns
  # output to see progress ---------------------------------
  country <- str_remove(x, "https://en.wikipedia.org//wiki/List_of_twin_towns_and_sister_cities_in_")
  print("Country: ---------------------------------------")
  print(country)
  print("         ---------------------------------------")
  # Main ---------------------------------
  sister_incountry <- x %>% read_html() %>% html_nodes("p") %>% html_text() %>% str_remove_all("[\\[\\]\\d]") %>% str_trim()
  # sister_incountry <- sister_incountry[(str_length(sister_incountry) %>% between(1, 30))]
  sister_incountry <- sister_incountry[c(3:length(sister_incountry))]
  aux <- x %>% read_html() %>% html_nodes("p, li") %>% html_text() %>% str_trim() %>% str_remove_all("[\\[\\]\\d]")
  positions <- which(aux %in% sister_incountry)
  begin_footnotes <- which(aux %>% str_detect("\\^")) %>% min() # final sister town right before the first footnote
  
  out <- tibble(sisterx = character(), sistery = character())
  for(i in 1:length(positions)){
    # i <- 1
    # output to see progress ---------------------------------
    print(aux[[positions[i]]])
    #  ---------------------------------
    start_position <- positions[i] + 1
    end_position <- positions[i+1]-1
    end_position <- ifelse(is.na(end_position), begin_footnotes - 1, end_position)
    out <- out %>% complete(sisterx = aux[[positions[i]]], sistery = aux[start_position:end_position])
  }
  return(list(out))
}
# sistertowns1 <- sistertowns[c(1),] %>% rowwise() %>% mutate(sisters = get_twintowns(href)) 
# sistertowns2 <- sistertowns[c(51:103),] %>% rowwise() %>% mutate(sisters = get_twintowns(href)) 
# sistertowns3 <- sistertowns[c(104:154),] %>% rowwise() %>% mutate(sisters = get_twintowns(href)) 

sistertowns <- sistertowns %>% rowwise() %>% mutate(sisters = get_twintowns(href)) 
sistertowns <-
  sistertowns %>% 
  unnest(sisters) %>% 
  rename(country.x = country, sistertown.x = sisterx) %>% 
  separate(sistery, c("sistertown.y", "country.y"), ",\\s", extra = "merge") %>% 
  select(sistertown.x, country.x, sistertown.y, country.y)

result <- sistertowns %>% 
  filter(country.x != country.y) %>% 
  drop_na() %>% 
  filter(!str_detect(sistertown.x, "terminated") & !str_detect(sistertown.x, "terminated")) %>%  
  count(country.x, country.y, sort = T)

# graph <- igraph::graph_from_data_frame(result, directed = F)

result_summarised <- tibble(country.x = character(), country.y = character(), count = integer())
result <- result %>% mutate(checked = 0)
for(i in 1:nrow(result)){
  print("Row: --------------")
  print(i)
  print("     --------------")
  # i <- 1
  if(result[i, 4] == 0){
  var1 <- result[[i, 1]]
  var2 <- result[[i, 2]]
  var3 <- result[[i, 3]]
  result[[i, 4]] <- 1
  var4 <- result %>% filter(checked == 0) %>% filter(country.y == var1 & country.x == var2) %>% pull(n)
  result[result$country.y == var1 & result$country.x == var2, 4] <- 1
  }
  if(!is_empty(var4)){
    var5 <- var3 + var4
    result_summarised <- result_summarised %>% complete(country.x = var1, country.y = var2, count = var5)
  }
  
}

result_summarised %>% arrange(desc(count))
result_summarised %>% filter(country.x == "Germany" & country.y == "France")
g <- graph_from_data_frame(result_summarised, directed = F) 
g <- delete_edges(g, E(g)[count < 250])
g <- delete_vertices(g, degree(g) == 0)
weights <- E(g)$count/ max(E(g)$count)
g %>% plot(vertex.label.color = "black", 
           edge.width = weights, edge.color = "black", layout = layout_nicely(g))
# save(sistertowns, file = "sistertowns.Rdata")




