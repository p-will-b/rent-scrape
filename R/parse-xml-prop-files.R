library(jsonlite)
library(httr)
library(xml2)
library(rvest)
library(tidyverse)

xml_files <- list.files("./data", pattern = ".xml", full.names = T)

# parsing function

html_json <- function(website_xml) {
  website_xml %>%
    html_nodes(xpath = "//script[@id='fusion-metadata']") %>%
    html_text()
}

parse_html_json <- function(json_text) {
  str_extract(json_text, "(?<=Fusion\\.globalContent\\=).+?(?=;Fusion\\.globalContentConfig)") %>%
    fromJSON()
}

get_prop_general_info <- function(json_list, col_names) {
  bind_rows(unlist(json_list[names(json_list) %in% col_names])) %>%
    rename(propertyName = name, propertyArea = area)
}

get_prop_unit_info <- function(json_list, element_name, cols_to_drop) {

  df_start <- json_list[[element_name]] %>%
    mutate(
      floorPlanId = floorPlan$name,
      propertyAddress = address$address
    )

  df_units <- df_start[, !colnames(df_start) %in% cols_to_drop] %>%
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

  prop_final <- bind_cols(df_start, df_units)
  return(prop_final)

}


parse_prop_xml <- function(website_xml_file, general_col_names, specific_cols_drop) {

  parsed_json <- html_json(website_xml = website_xml_file) %>%
    parse_html_json()

  get_prop_general_info()

}

t0 <- read_html(xml_files[4])

t1 <- t0 %>%
  html_nodes(xpath = "//script[@id='fusion-metadata']") %>%
  html_text()

t2 <- stringr::str_extract(t1, "(?<=Fusion\\.globalContent\\=).+?(?=;Fusion\\.globalContentConfig)")

t3 <- fromJSON(t2)

prop_df <- bind_rows(unlist(t3[names(t3) %in% c("communityId", "name", "area", "geoLocation", "phone", "email")])) %>%
  rename(propertyName = name, propertyArea = area)

prop_data <- t3[["units"]] %>%
  mutate(
    floorPlanId = floorPlan$name,
    propertyAddress = address$address
  )

prop_data_1 <- prop_data[, !colnames(prop_data) %in% c("floorPlan", "address", "virtualTour", "characteristics", "promotions")] %>%
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

prop_final <- bind_cols(prop_df, prop_data)


t0 <-

t2 <- stringr::str_extract(t1, "(?<=Fusion\\.globalContent\\=).+?(?=;Fusion\\.globalContentConfig)")

t3 <- fromJSON(t2)

prop_df <- bind_rows(unlist(t3[names(t3) %in% c("communityId", "name", "area", "geoLocation", "phone", "email")])) %>%
  rename(propertyName = name, propertyArea = area)

prop_data <- t3[["units"]] %>%
  mutate(
    floorPlanId = floorPlan$name,
    propertyAddress = address$address
  )

prop_data_1 <- prop_data[, !colnames(prop_data) %in% c("floorPlan", "address", "virtualTour", "characteristics", "promotions")] %>%
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

prop_final <- bind_cols(prop_df, prop_data)

parse_prop_xml <- function(website_xml_file) {
  html_json(website_xml = website_xml_file) %>%
    parse_html_json() %>%
    get_prop_general_info()
}
