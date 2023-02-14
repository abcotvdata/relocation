savezipstuff
# get population
my_vars <- c(population = "B01003_001")

# Get the total population and total number of housing units for each census tract
zips <- get_acs(geography = "zcta",
                variables = my_vars,
                survey = "acs5",
                year = 2021,
                geometry = TRUE) %>%
  rename("population"="estimate") %>% janitor::clean_names()

zip_centroid <- zips %>% 
  st_transform(2273) %>% # convert to projected coord system for better centroid
  st_centroid()