library(rvest)
library(tidyverse)
library(data.table)
library(reshape2)
library(RCurl)
library(stringr)
library(XML)

# get list of links that go to the main press release pages
link_list <- function(url) {
  test_link <- read_html(url) %>% 
    html_nodes(xpath = "//div[@class = 'resource-listing__content']/a[@href]") %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    mutate(href = paste0("https://www.hrc.org",href))
  return(test_link)
}

# Get main press release text in link
press_text <- function(df_links) {
  test_text <- read_html(df_links) %>%
    html_nodes(xpath = "//div[@class = 'entry wysiwyg']")
  
  xml_remove(html_nodes(test_text, xpath = "//em"))
  
  test_text <- html_text(test_text, trim = T)
  return(test_text)
}

# extract press release text and bind to dataframe
press_content <- function(url) {
  press_links <- link_list(url) %>%
    mutate(text = pmap(list(href), press_text))
  return(press_links)
}

# retrieve dates
press_dates <- function(url) {
  get_dates <- read_html(url) %>% 
    html_nodes(xpath = "//div[@class = 'resource-listing__date']") %>%
    html_text()
  all_dates <- data.frame(t(rbind(get_dates)))
  return(all_dates)
}
