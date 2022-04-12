library(tidyverse)

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

