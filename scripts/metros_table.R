library(tidyverse)
library(tidycensus)
library(readxl)

my_vars <- c(population = "B01003_001")

zips <- get_acs(geography = "zcta",
variables = my_vars,
survey = "acs5",
year = 2021,
geometry = FALSE) %>%
rename("population"="estimate") %>% janitor::clean_names()



zip2metro <- read_excel("ZIP_CBSA_122021.xlsx") 
zip2metro[5:8] <- round(zip2metro[5:8],2)

zip2metro <- zip2metro %>% 
  filter(cbsa!="99999") %>% 
  filter(usps_zip_pref_state %in% my_states) %>%
  filter(tot_ratio > .51)



metro2dma <- read_csv("/Volumes/Jarvis/R/localizer20/msa_county_dma_crosswalk2020.csv", 
                      col_types = cols(cbsa_code = col_character(), 
                                       csa_code = col_character(), metropolitan_micropolitan_statistical_area = col_skip(), 
                                       metropolitan_division_title = col_skip(), 
                                       csa_title = col_skip()))

metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="10900","NEW YORK",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="16980","CHICAGO",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="35620","NEW YORK",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="36837","CHICAGO",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="37980","PHILADELPHIA",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="40260","RALEIGH",metro2dma$DMA)
metro2dma$DMA <- ifelse(metro2dma$cbsa_code=="41940","SAN FRANCISCO",metro2dma$DMA)

metro2dma <- metro2dma %>%
  group_by(cbsa_code,cbsa_title,DMA) %>% 
  summarise(count=n())

zip2metro <- left_join(zip2metro %>% select(1:4),metro2dma %>% select(1:3),by=c("cbsa"="cbsa_code"))
