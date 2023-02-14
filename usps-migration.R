library(tidyverse)
library(readxl)

# download 
usps2022 <- read_csv("https://about.usps.com/who/legal/foia/documents/change-of-address-stats/Y2022.csv")
usps2021 <- read_csv("https://about.usps.com/who/legal/foia/documents/change-of-address-stats/Y2021.csv")
usps2020 <- read_csv("https://about.usps.com/who/legal/foia/documents/change-of-address-stats/Y2020.csv")
usps2019 <- read_csv("https://about.usps.com/who/legal/foia/documents/change-of-address-stats/Y2019.csv")
usps2018 <- read_csv("https://about.usps.com/who/legal/foia/documents/change-of-address-stats/Y2018.csv")

# quick-create list of states for filtering
my_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# combine five years and clean
relos <- rbind(usps2018,usps2019,usps2020,usps2021,usps2022) %>% janitor::clean_names()
relos$year <- str_sub(relos$yyyymm,1,4)
relos$zipcode <- sub('="','',relos$zipcode)
relos$zipcode <- sub('"','',relos$zipcode)
# get ride of territories
relos <- relos %>% filter(state %in% my_states)

relos <- left_join(relos,zip2metro %>% select(1,2,5,6),by=c("zipcode"="zip"))

relos_yearly <- relos %>%
  group_by(year,zipcode,city,state) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
relos_yearly$pct_inbound <- relos_yearly$inbound/(relos_yearly$inbound+relos_yearly$outbound)

relos_total <- relos %>%
  group_by(zipcode,city,state) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
relos_total$pct_inbound <- relos_total$inbound/(relos_total$inbound+relos_total$outbound)

cityrelos_yearly <- relos %>%
  group_by(year,city,state) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
cityrelos_yearly$pct_inbound <- cityrelos_yearly$inbound/(cityrelos_yearly$inbound+cityrelos_yearly$outbound)

cityrelos_total <- relos %>%
  group_by(city,state) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
cityrelos_total$pct_inbound <- cityrelos_total$inbound/(cityrelos_total$inbound+cityrelos_total$outbound)


metrorelos_yearly <- relos %>%
  group_by(year,cbsa_title,DMA) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
metrorelos_yearly$pct_inbound <- metrorelos_yearly$inbound/(metrorelos_yearly$inbound+metrorelos_yearly$outbound)

metrorelos_total <- relos %>%
  group_by(cbsa_title,DMA) %>%
  summarise(outbound = sum(as.numeric(total_from_zip)),
            inbound = sum(as.numeric(total_to_zip)),
            netgain = inbound - outbound)
metrorelos_total$pct_inbound <- metrorelos_total$inbound/(metrorelos_total$inbound+metrorelos_total$outbound)

metrorelos_total %>% write_csv("data/metro_areas_total.csv")
metrorelos_yearly %>% write_csv("data/metro_areas_yearly.csv")
cityrelos_total %>% write_csv("data/cities_total.csv")
cityrelos_yearly %>% write_csv("data/cities_yearly.csv")
ziprelos_total %>% write_csv("data/zips_areas_total.csv")
ziprelos_yearly %>% write_csv("data/zips_areas_yearly.csv")
