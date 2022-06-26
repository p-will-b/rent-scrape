library(jsonlite)
library(httr)
library(xml2)
library(tidyverse)

# create randomized time

rand_time <- function(n, st = Sys.Date(), et = Sys.Date() + 1, tz = "GMT") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et, st, unit="sec"))
  ev <- sort(runif(n, 0, dt))
  rt <- st + ev
  paste(format(rt, "%a, %d %b %Y %H:%M:%S"), tz)

}

# get links to search

prop_links <- read_csv("./data/avalon-prop-links.csv", col_types = cols())
prop_links <- sample_n(prop_links, 3) %>%
  bind_rows(prop_links %>% filter(property == "ava-arts-district")) %>%
  distinct()

# issue get call to retrieve website

get_avalon_website <- possibly(

  function(target_link) {

    # get randomized UA

    ual <- readLines("./assets/ua.txt")
    ual <- ual[runif(1, min = 1, max = length(ual))]

    # build query

    headers <- c(
      `authority` = 'api.avalonbay.com',
      `accept` = '*/*',
      `accept-language` = 'en-US,en;q=0.9',
      `dnt` = '1',
      `if-modified-since` = rand_time(n = 1, tz = "GMT"),
      `origin` = 'https://www.avaloncommunities.com',
      `referer` = 'https://www.avaloncommunities.com/',
      `sec-fetch-dest` = 'empty',
      `sec-fetch-mode` = 'cors',
      `sec-fetch-site` = 'cross-site',
      `sec-gpc` = '1',
      `user-agent` = ual
    )

    res <- GET(url = target_link, add_headers(.headers = headers), user_agent(ual))
    res_content <- content(res)
    return(res_content)

  },
  otherwise = xml_new_document()
)

write_prop_xml <- function(xml_doc, property_name) {
  write_xml(xml_doc, sprintf("./data/%s_%s-prop-data.xml", Sys.Date(), property_name))
}

prop_content <- walk(
  1:nrow(prop_links), ~ {
    get_avalon_website(target_link = prop_links$links[.x]) %>%
      write_prop_xml(property_name = prop_links$property[.x])
    }
  )
