
library(dplyr)
library(rvest)
library(glue)
library(purrr)
library(xml2)

wektorLinkow<-c()
for(str in 1:3) {
  newUrl<- glue("https://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str={str}")
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[7]/article/section/div/div[1]/ul/li/a')
  wektorLinkow<-c(wektorLinkow, xml_attr(result,"href"))
}
wektorLinkow

wektorLinkow <- wektorLinkow[grepl("https://wiadomosci.gazeta.pl/wiadomosci/7,", wektorLinkow)]
wektorLinkow





get_article_details <- function(art_url) {
  # próbujemy pobrać stronę
  page <- read_html(art_url)
  

  # tytuł - to H1 na stronie
  tytul <- page %>%
    html_node(xpath = "//h1") %>%
    html_text() %>%
    trimws()
  

    
  # autor
  autor <- page %>%
    html_node(xpath = "//div[@class='author_and_date']//span[@class='article_author']") %>%
    html_text() %>%
    trimws()
  
  # data publikacji
  data <- page %>%
    html_node(xpath = "//div[@class='author_and_date']//span[@class='article_date']") %>%
    html_text() %>%
    trimws()
  
  # lead
  lead <- page %>%
    html_node(xpath = "//div[@id='gazeta_article_lead']") %>%
    html_text() %>%
    trimws()
  
    
  # pakujemy to w 1-wierszowa tabele
  article <- tibble(url = art_url, title = tytul, author = autor, date = data, lead = lead)
  
  return(article)
}



articles <- wektorLinkow %>%
  map_df(get_article_details)
