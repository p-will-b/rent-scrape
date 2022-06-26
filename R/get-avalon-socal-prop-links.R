library(rvest)
library(tidyverse)

socal_pg <- read_html("https://www.avaloncommunities.com/southern-california")
avalon_prop_links <- socal_pg %>%
  html_nodes(".community-search-results-list") %>%
  html_nodes(xpath = "//li[@class='community-item']") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[!grepl("book-a-tour|contact-us", .)] %>%
  paste0("https://new.avaloncommunities.com", .) %>%
  tibble(links = .) %>%
  mutate(property = str_extract(links, "[^/]+$"))

write_csv(avalon_prop_links, "./data/avalon-prop-links.csv")
