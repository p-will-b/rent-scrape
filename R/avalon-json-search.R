library(jsonlite)
library(httr)

# create randomized time

rand_time <- function(n, st = Sys.Date(), et = Sys.Date() + 1, tz = "GMT") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et, st, unit="sec"))
  ev <- sort(runif(n, 0, dt))
  rt <- st + ev
  paste(format(rt, "%a, %d %b %Y %H:%M:%S"), tz)

}

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
  #`if-none-match` = '"1649761573:dtagent10237220328075400smrd"',
  `origin` = 'https://www.avaloncommunities.com',
  `referer` = 'https://www.avaloncommunities.com/',
  `sec-fetch-dest` = 'empty',
  `sec-fetch-mode` = 'cors',
  `sec-fetch-site` = 'cross-site',
  `sec-gpc` = '1',
  `user-agent` = ual
)

params <- list(
  `cityArea` = '513',
  `floorPlanType` = '1BD',
  `min` = '0',
  `max` = '15000',
  `moveInDate` = ''
)

# issue get, store and write results

av_res <- GET(url = 'https://api.avalonbay.com/communitysearch.json', add_headers(.headers = headers), query = params, user_agent(ual))
av_content <- content(av_res)
write_json(av_content[["results"]], sprintf("./data/%s_avalon-listings.json", Sys.Date()))
