library(tidyverse)
library(jsonlite)
library(httr)

# get randomized time

rand_time <- function() {

  ra <- runif(1, min = 35000, max = 33000)



}

rand_time <- function(n, st = Sys.Date(), et = Sys.Date() + 1, tz = "GMT") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et, st, unit="sec"))
  ev <- sort(runif(n, 0, dt))
  rt <- st + ev
  paste(format(rt, "%a, %d %b %Y %H:%M:%S"), tz)
}


rand_time(n = 1)

# build query

headers <- c(
  `authority` = 'api.avalonbay.com',
  `accept` = '*/*',
  `accept-language` = 'en-US,en;q=0.9',
  `dnt` = '1',
  `if-modified-since` = 'Tue, 12 Apr 2022 11:06:12 GMT',
  `if-none-match` = '"1649761573:dtagent10237220328075400smrd"',
  `origin` = 'https://www.avaloncommunities.com',
  `referer` = 'https://www.avaloncommunities.com/',
  `sec-fetch-dest` = 'empty',
  `sec-fetch-mode` = 'cors',
  `sec-fetch-site` = 'cross-site',
  `sec-gpc` = '1',
  `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.79 Safari/537.36'
)

params <- list(
  `cityArea` = '513',
  `floorPlanType` = '1BD',
  `min` = '0',
  `max` = '15000',
  `moveInDate` = ''
)

res <- GET(url = 'https://api.avalonbay.com/communitysearch.json', httr::add_headers(.headers=headers), query = params)
