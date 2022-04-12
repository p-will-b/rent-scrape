# load libs

library(rvest)
library(dplyr)
library(stringr)

# read links

imp_ac <- read_html("https://new.avaloncommunities.com/california/calabasas-apartments/avalon-calabasas/?bedroom=2BD")
imp_apas <- read_html("https://www.avaloncommunities.com/connecticut/norwalk-apartments/avalon-east-norwalk/apartments?bedroom=1BD")
imp_av1 <- read_html("https://new.avaloncommunities.com/california/los-angeles-apartments/avalon-playa-vista/?bedroom=1BD")

# process and output

avalon_processor <- function(raw_html) {

  txt <- raw_html %>%
    html_nodes(".content") %>%
    html_text() %>%
    tolower()

  txt_proc <- txt[!grepl("unavailable", txt)]

  if(length(txt_proc) == 0) {

    txt_proc <- "no listings"

  }

  return(txt_proc)
}

# process data

ac_proc <- avalon_processor(imp_ac)
aen_proc <- avalon_processor(imp_aen)

# data for saving

data_out <- data.frame(
  as_of_date = Sys.Date(),
  location = c(rep("calabasas", length(ac_proc)), rep("east norwalk", length(aen_proc))),
  listings = c(ac_proc, aen_proc)
)

# write to csv

write.csv(data_out, sprintf("./data/%s_avalon-listings.csv", Sys.Date()), row.names = FALSE)
