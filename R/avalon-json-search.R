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

# NOW DO WEBSITES ---------------------------------------------------------

# get links to search

prop_links <- read_csv("./data/avalon-prop-links.csv", col_types = cols())

# issue get call to retrieve website

get_avalon_website <- possibly(

  function(target_link) {

    # get randomized UA

    new_ual <- readLines("./assets/ua.txt")
    new_ual <- ual[runif(1, min = 1, max = length(new_ual))]

    # build query

    new_headers <- c(
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

    res <- GET(url = target_link, add_headers(.headers = new_headers), user_agent(new_ual))
    res_content <- content(res)
    return(res_content)

  },
  otherwise = xml_new_document()
)


write_prop_xml <- function(xml_doc, property_name) {
  write_xml(xml_doc, sprintf("./data/%s_%s-prop-data.xml", Sys.Date(), property_name))
}

save_prop_rds <- function(df, property_name) {
  saveRDS(df, sprintf("./data/%s_%s-prop-data.rds", Sys.Date(), property_name))
}

# parsing function

html_json <- function(website_xml) {
  website_xml %>%
    read_html() %>%
    html_nodes(xpath = "//script[@id='fusion-metadata']") %>%
    html_text()
}

parse_html_json <- function(json_text) {
  str_extract(json_text, "(?<=Fusion\\.globalContent\\=).+?(?=;Fusion\\.globalContentConfig)") %>%
    fromJSON()
}

get_prop_unit_info <- function(json_list, element_name, col_names_general, cols_to_drop_unit) {

  general_info <- bind_rows(unlist(json_list[names(json_list) %in% col_names_general])) %>%
    rename(propertyName = name, propertyArea = area)

  df_start <- json_list[[element_name]] %>%
    mutate(
      floorPlanId = floorPlan$name,
      propertyAddress = address$address
    )

  df_units <- df_start[, !colnames(df_start) %in% cols_to_drop_unit] %>%
    unnest(cols = lowestPricePerMoveInDate, names_sep = "_") %>%
    unnest(cols = unitRentPrice) %>%
    unnest(cols = pricesPerMoveinDate) %>%
    unnest(pricesPerTerms, names_sep = "_") %>%
    pivot_longer(matches("pricesPerTerms_\\d+"), names_to = "leaseTerm", values_to = "data") %>%
    unnest(data) %>%
    mutate(
      leaseTerm = as.numeric(gsub("pricesPerTerms_", "", leaseTerm, fixed = T)),
      across(c(availableDate, moveInDate, furnishedAvailableDate, lowestPricePerMoveInDate_date), ~ lubridate::mdy(gsub("(\\d+\\/\\d+/\\d+)(.*)", "\\1", .x)))
    )

  prop_final <- bind_cols(general_info, df_units)
  return(prop_final)

}

# safe version

parse_prop_xml <- function(website_xml_file, element_name, col_names_general, cols_to_drop_unit) {

  parsed_json <- html_json(website_xml = website_xml_file) %>%
    parse_html_json()

  get_prop_unit_info(
    parsed_json,
    element_name,
    col_names_general,
    cols_to_drop_unit
  ) %>%
    mutate(as_of_date = as.Date(str_extract(website_xml_file, "\\d+-\\d+-\\d+")))

}

possibly_parse_prop_xml <- possibly(parse_prop_xml, otherwise = tibble())

# loop through, write xml, save rds

walk(
  1:nrow(prop_links), ~ {
    avw <- get_avalon_website(target_link = prop_links$links[.x])
    write_prop_xml(xml_doc = avw, property_name = prop_links$property[.x])
    parse_prop_xml(
      website_xml_file = avw,
      element_name = "units",
      col_names_general = c("communityId", "name", "area", "geoLocation", "phone", "email"),
      cols_to_drop_unit = c("floorPlan", "address", "virtualTour", "characteristics", "promotions")
    ) %>%
      save_prop_rds()
    avw <- NULL

  }
)


