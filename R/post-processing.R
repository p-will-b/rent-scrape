library(tidyverse)
library(jsonlite)
library(fst)
library(RcppSimdJson)
library(data.table)

# import

impd <- list.files("d:/data/rentscrape-data/raw", pattern = ".csv", full.names = T)
dat <- map_dfr(impd, read_csv, col_types = cols())

# clean

listing_dat <- dat %>%
  mutate(
    unit = str_extract(listings, "(?<=apartment ).+?(?= bedrooms)"),
    px = str_extract(listings, "(?<=sqft\\$).+?(?= w)"),
    sqft = str_extract(listings, "\\d+(?= sqft)"),
    n_bed = str_extract(listings, "\\d+(?= bedrooms)"),
    n_bath = str_extract(listings, "\\d+(?= baths)"),
    finish_package = str_extract(listings, "^.+?(?=apartment)")
    ) %>%
  mutate(
    px = parse_number(px),
    n_bed = ifelse(nchar(n_bed) > 1, str_sub(n_bed, start = -1), n_bed)
  ) %>%
  type_convert(col_types = cols()) %>%
  mutate(px_sf = px / sqft)

# look at average listing price by unit type

avgs <- listing_dat %>%
  filter(location == "calabasas") %>%
  group_by(as_of_date, finish_package) %>%
  summarize(
    across(c(px, px_sf), mean),
    .groups = "keep"
    ) %>%
  group_by(as_of_date) %>%
  mutate(avg_px_sf = mean(px_sf))

ggplot(avgs, aes(as_of_date, avg_px_sf)) +
  geom_line() +
  scale_x_date(date_breaks = "2 months")

# now json files

impj <- list.files("d:/data/rentscrape-data/raw", pattern = ".json", full.names = T)
impo <- vector(mode = "list", length = length(impj))

for(i in seq_along(impj)) {

  ji <- fromJSON(impj[i], flatten = T, simplifyDataFrame = T)

  # find cols which are not really lists

  for(n in names(which(unlist(sapply(ji, \(x) unique(lengths(x)))) == 1))) {
    ji[[n]] <- unlist(ji[[n]])
  }

  impo[[i]] <- ji %>%
    unnest_legacy(cols = c(floorPlanTypes)) %>%
    select(
      communityName, communityAddress = communityAddress.address1,
      communityCity = communityAddress.city, numberOfPromotions,
      floorPlanTypeCode, effectiveRent, maxEffectiveRent, showAsAvailable,
      hasPromotion = hasPromotion...31
    ) %>%
    mutate(
      across(where(is.list), unlist),
      as_of_date = as.Date(str_extract(impj[i], "\\d{4}-\\d{2}-\\d{2}"))
        ) %>%
    as_tibble() %>%
    filter(showAsAvailable)

}

avalon_listings <- bind_rows(impo)

t1 <- fromJSON(impj[76], flatten = T, simplifyDataFrame = T)

pg

avalon_listings %>%
  count(communityName) %>%
  print(n=Inf)

avalon_listings %>%
  filter(communityName == "Avalon Playa Vista")

avalon_listings %>%
  filter(floorPlanTypeCode == "1BD") %>%
  add_count(communityName) %>%
  filter(n > 70) %>%
  ggplot(aes(effectiveRent, communityName, color = hasPromotion)) +
  geom_boxplot()

avalon_listings %>%
  filter(floorPlanTypeCode == "1BD") %>%
  add_count(communityName) %>%
  filter(n > 70) %>%
  group_by(as_of_date, communityName) %>%
  summarize(
    rent = mean(effectiveRent),
    .groups = "drop"
    ) %>%
  ggplot(aes(as_of_date, rent, color = communityName)) +
  geom_line(show.legend = F) +
  facet_wrap(~ communityName, scales = "free")


avalon_listings %>%
  filter(communityName == "AVA Studio City I" & floorPlanTypeCode == "1BD") %>%
  ggplot(aes(as_of_date, effectiveRent, color = hasPromotion)) +
  geom_point()

avalon_listings %>%
  group_by(as_of_date, floorPlanTypeCode) %>%
  summarize(
    avg = mean(effectiveRent),
    sd = sd(effectiveRent)
  ) %>%
  ggplot(aes(as_of_date, avg, color = floorPlanTypeCode)) +
  geom_line()

##### PARSING INDIVIDUAL PROPS

last_fst <- sort(
  list.files("d:/data/rentscrape-data/processed", pattern = ".fst", full.names = T),
  decreasing = T
  )[1]

listing_data <- read_fst(last_fst)
setDT(listing_data, key = c("as_of_date", "propertyName"))

pv <- listing_data[propertyName == "Avalon Playa Vista"]
setnames(
  pv,
  old = c("name", "lowestPricePerMoveInDate_netEffectivePrice", "lowestPricePerMoveInDate_termLength"),
  new = c("unitNo", "lowestPx", "lowestPxLeaseTerm")
  )
pv[, floor := str_extract(unitNo, "\\d")]
upv <- unique(pv, by = c("as_of_date", "unitNo", "lowestPx", "leaseTerm"))
summary(lm(netEffectivePrice ~ bedroom + bathroom + floor + leaseTerm, data = upv))

upv[
  floor == "4" & bedroom == 1,
  .(minPx = median(netEffectivePrice))
  , by = .(as_of_date)
  ] %>%
  ggplot(aes(as_of_date, minPx)) +
  geom_line()

# read json

jsons <- sort(
  list.files("d:/data/rentscrape-data/raw", pattern = ".json", full.names = T),
  decreasing = T
)

csvs <- sort(
  list.files("d:/data/rentscrape-data/raw", pattern = ".csv", full.names = T),
  decreasing = T
)

# get calabasas csv data

calabasas_csv <- read_csv(csvs, col_types = cols())
setDT(calabasas_csv)
calabasas_data <- calabasas_csv[location == "calabasas" & listings != "no listings"]
calabasas_data[, px := parse_number(str_extract(listings, "(?<=\\$)[0-9,]+"))]
cbas2b2bcsv <- calabasas_data[, .(medPx = median(px)), by = "as_of_date"]

# get calabasas data

get_calabasas_json <- function(json_orig) {
  inj <- fparse(paste(readLines(json_orig), collapse = "\n"))
  setDT(inj)
  inj[communityCode == "CA541", floorPlanTypes][[1]]
}

calabasas_list <- lapply(jsons, get_calabasas_json)
names(calabasas_list) <- str_extract(jsons, "[0-9-]{4,}")
calabasas_df <- rbindlist(calabasas_list, use.names = TRUE, idcol = "as_of_date")
calabasas_df[, as_of_date := as.Date(as_of_date)]
calabasas_df[, names(calabasas_df) := lapply(.SD, unlist, use.names = FALSE)]
cbas2b2bjson <- calabasas_df[
  display == "2 bedrooms" & showAsAvailable == TRUE,
  .(medPx = median(effectiveRent)),
  by = "as_of_date"
]

cbas2b2b <- rbindlist(list(cbas2b2bcsv, cbas2b2bjson))
ggplot(cbas2b2b, aes(as_of_date, medPx)) +
  geom_line()

#

unique(cbas2b2b)

pv %>%
  mutate(floor = str_extract(name, "\\d")) %>%
  distinct(as_of_date, unitNo = name, bedroom, bathroom, lowestPx = lowestPricePerMoveInDate_netEffectivePrice, leaseTerm = lowestPricePerMoveInDate_termLength)

min_px <- listing_data %>%
  select(
    propertyArea, propertyName, as_of_date, unitNo = name, bedroom, bathroom, lowestPx = lowestPricePerMoveInDate_netEffectivePrice, leaseTerm = lowestPricePerMoveInDate_termLength
  ) %>%
  mutate(floor = str_extract(unitNo, "\\d")) %>%
  distinct() %>%
  group_by(propertyName) %>%
  filter(floor == max(floor) & bedroom == 1) %>%
  mutate(bed_bath = sprintf("%s bed: %s bath", bedroom, bathroom)) %>%
  group_by(as_of_date, propertyName) %>%
  arrange(lowestPx) %>%
  filter(row_number() == 1) %>%
  ungroup()

areas_not <- c("camarillo", "mission-viejo", "glendora", "ranch-santa-margarita", "san-diego", "lake-forest", "seal-beach", "vista", "irvine", "pomona")

min_px %>%
  filter(propertyName == "Avalon Playa Vista") %>%
  ggplot(aes(as_of_date, lowestPx)) +
  geom_line() +
  facet_wrap(~ propertyName)

listing_data %>%
  filter(propertyName == "Avalon Playa Vista" & as_of_date == "2022-09-10") %>%
  count(name)

min_px %>%
  filter(propertyName == "Avalon Playa Vista") %>%
  arrange(desc(as_of_date)) %>%
  print(n=Inf)


min_px %>%
  filter(propertyArea %in% "vista") %>%
  ggplot(aes(as_of_date, lowestPx)) +
  geom_line() +
  facet_wrap(~ propertyName)


hist_csv <- listing_dat %>%
  filter(location == "calabasas") %>%
  group_by(as_of_date) %>%
  summarize(effectiveRent = min(px))

hist_json <- avalon_listings %>%
  filter(communityName == "Avalon Calabasas" & floorPlanTypeCode == "2BD" & showAsAvailable) %>%
  select(as_of_date, effectiveRent)

all_cbas <- bind_rows(hist_csv, hist_json) %>%
  group_by(as_of_date) %>%
  summarize(minRent = mean(effectiveRent))

ggplot(all_cbas, aes(as_of_date, minRent)) +
  geom_line()


avalon_listings %>%
  filter(floorPlanTypeCode == "1BD") %>%
  add_count(communityName)


min_px %>%
  count(propertyArea)
