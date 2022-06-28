library(jsonlite)
library(httr)
library(xml2)
library(rvest)
library(tidyverse)
library(lubridate)

xml_files <- list.files("./data", pattern = ".xml", full.names = T)

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


parse_prop_xml <- function(website_xml_file, element_name, col_names_general, cols_to_drop_unit) {

  parsed_json <- html_json(website_xml = website_xml_file) %>%
    parse_html_json()

  get_prop_unit_info(
    parsed_json,
    element_name,
    col_names_general,
    cols_to_drop_unit
  ) %>%
    mutate(as_of_date = ymd(str_extract(website_xml_file, "\\d+-\\d+-\\d+")))

}
