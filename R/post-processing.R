library(tidyverse)
library(jsonlite)

# import

impd <- list.files("./data", pattern = ".csv", full.names = T)
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

impj <- list.files("./data", pattern = ".json", full.names = T)
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

