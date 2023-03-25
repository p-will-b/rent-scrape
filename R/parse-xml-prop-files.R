library(jsonlite)
library(httr)
library(xml2)
library(rvest)
library(tidyverse)
library(lubridate)
library(fst)
library(data.table)

# string literals

raw_data_path <- "d:/data/rentscrape-data/raw"
proc_data_path <- "d:/data/rentscrape-data/processed"
xpath_node <- "//script[@id='fusion-metadata']"
json_regex <-
  "(?<=Fusion\\.globalContent\\=).+?(?=;Fusion\\.globalContentConfig)"
unicode_regex <- "\\\\\\d+\\[\\d.+?\\dm"
elemName <- "units"
colsGeneral <-
  c("communityId", "name", "area", "geoLocation", "phone", "email")
cols2dropUnit <-
  c(
    "floorPlan",
    "address",
    "virtualTour",
    "characteristics",
    "promotions"
  )

# parsing function

html_json <- function(website_xml) {
  read_html(website_xml) %>%
    html_nodes(xpath = xpath_node) %>%
    html_text()
}

parse_html_json <- function(json_text) {
  fromJSON(str_extract(json_text, json_regex))
}

general_bldg_info <- function(json_list, col_names_general) {
  tryCatch(
    {
      general_info <- bind_rows(
        unlist(
          json_list[names(json_list) %in% col_names_general]
          )
        ) %>%
        rename(
          propertyName = name,
          propertyArea = area,
          latitude = geoLocation.latitude,
          longitude = geoLocation.longitude
          )
      return(general_info)
    },
    error = \(e) {
      general_info <- list(
        "propertyName" = json_list$name,
        "errorMsg" = e
      )
      return(general_info)
    }
  )
}

prop_unit_info <- function(json_list,
                           element_name,
                           cols_to_drop_unit,
                           general_info_df) {
    tryCatch(
      {
        df_start <- json_list[[element_name]] %>%
          mutate(
            floorPlanId = floorPlan$name,
            propertyAddress = address$address
          )

        wanted_cols <- !colnames(df_start) %in% cols_to_drop_unit

        df_units <- df_start[, wanted_cols] %>%
          unnest(cols = lowestPricePerMoveInDate, names_sep = "_") %>%
          unnest(cols = unitRentPrice) %>%
          unnest(cols = pricesPerMoveinDate) %>%
          unnest(pricesPerTerms, names_sep = "_") %>%
          pivot_longer(
            matches("pricesPerTerms_\\d+"),
            names_to = "leaseTerm",
            values_to = "data"
          ) %>%
          unnest(data) %>%
          mutate(
            leaseTerm = as.numeric(gsub(
              "pricesPerTerms_", "", leaseTerm,
              fixed = T
            )),
            across(
              c(
                availableDate,
                moveInDate,
                furnishedAvailableDate,
                lowestPricePerMoveInDate_date
              ),
              ~ mdy(gsub("(\\d+\\/\\d+/\\d+)(.*)", "\\1", .x))
            )
          )

        df_out <- bind_cols(general_info_df, df_units)

        return(df_out)
      },
      error = \(e) {
        em <- conditionMessage(e)
        if (grepl("applied to an object of class.+?(NULL|list)", em)) {
          stop(sprintf(
            "Element 'unit' is likely missing from %s",
            json_list$name
          ))
        } else {
          list(errorCondition(e), json_list$name)
        }
      }
    )
  }

# put all functions together

parse_prop_xml <- safely(
  function(website_xml_file,
           element_name,
           col_names_general,
           cols_to_drop_unit) {
    pj <- html_json(website_xml = website_xml_file) %>%
      parse_html_json()

    gi <- general_bldg_info(pj, col_names_general)

    ui <- prop_unit_info(
      json_list = pj,
      element_name = element_name,
      cols_to_drop_unit = cols_to_drop_unit,
      general_info_df = gi
    ) %>%
      mutate(
        as_of_date = ymd(
          str_extract(website_xml_file, "\\d+-\\d+-\\d+")
          )
      )
  }
)
# find the last RDS and only read files after that

path_dates <- function(yyyy_mm_dd_path, find_last = FALSE) {
  dts <- as.Date(str_extract(yyyy_mm_dd_path, "\\d{4}[0-9-]+"))
  if(find_last) {
    max(dts)
  } else {
    dts
  }
}

# using RDS write dats, find relevant xml files, parse, & keep only those where succeeded

xml_files <- list.files(raw_data_path, pattern = ".xml", full.names = TRUE)
fst_files <- list.files(proc_data_path, pattern = ".fst", full.names = TRUE)
last_fst <- path_dates(fst_files, find_last = TRUE)
xml_unproc <- xml_files[path_dates(xml_files) > last_fst]

n_files <- length(xml_unproc)
parsed_xmls <- vector(mode = "list", length = n_files)
for(i in seq_along(parsed_xmls)) {
  parsed_xmls[[i]] <- parse_prop_xml(
    website_xml_file = xml_unproc[i],
    element_name = elemName,
    col_names_general = colsGeneral,
    cols_to_drop_unit = cols2dropUnit
  )
  message(paste0(i, " of ", n_files))
}

listing_data <- parsed_xmls[sapply(sapply(parsed_xmls, "[[", "error"), is.null)]
old_listing_data <- read_fst(sprintf("d:/data/rentscrape-data/processed/%s_listing-data.fst", last_fst))
new_ld <- bind_rows(lapply(listing_data, "[[", "result"))
all_listing_data <- bind_rows(old_listing_data, new_ld)
all_listing_data$finishPackage <- NULL
last_xml <- path_dates(xml_unproc, find_last = TRUE)
write_fst(all_listing_data, paste0(proc_data_path, "/", last_xml, "_listing-data.fst"))
setDT(all_listing_data, key = c("as_of_date", "propertyName"))

# analyze

rentProp <- all_listing_data[
  , .(median_rent = median(netEffectivePrice, na.rm = TRUE)),
  by = c("as_of_date", "propertyName")
]

ggplot(rentProp[propertyName == "Avalon Playa Vista"], aes(as_of_date, median_rent)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 month")
