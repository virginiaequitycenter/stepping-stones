# Stepping Stones Data: Arrests, Alcohol Related ----
# Updated 2023-02-11
# Contributor: Michele Claibourn
# Acquire data from Crime in Virginia Publications
# https://vsp.virginia.gov/sections-units-bureaus/bass/criminal-justice-information-services/uniform-crime-reporting/
# and from https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia State Police, Data Analysis and Reporting Team, "Crime in Virginia." 1999-2021.
# https://vsp.virginia.gov/sections-units-bureaus/bass/criminal-justice-information-services/uniform-crime-reporting/

# manually entered from reports
# Charlottesville and Albemarle Tables: NIBRS Agency Crime Overview
# sum driving under the influence, liquor law violations, drunkeness
# Statewide Tables: Number of Arrestees by Offense & Age Range
# sum driving under the influence, liquor law violations, drunkeness


# Libraries ----
library(tidyverse)

# Read in csv file ----
arr_alc <- read_csv("datadownloads/arrests_alcohol.csv")

arr_alc <- arr_alc %>% 
  mutate(fips = case_when(
           locality == "Albemarle" ~ "51003",
           locality == "Charlottesville" ~ "51540",
           locality == "Virginia" ~ "51"
         ))

# Read in population data ----
pop <- read_csv("data/pop_data_cdc.csv")

pop <- pop %>% 
  select(fips, year, pop_1019) %>% 
  mutate(fips = as.character(fips))

# join data frames
arr_alc <- left_join(arr_alc, pop)

# fill in pop data for 2021 and generate rate
arr_alc <- arr_alc %>% 
  group_by(locality) %>% 
  fill(pop_1019, .direction = "up") %>% 
  ungroup()

# create rate per 1000
arr_alc <- arr_alc %>% 
  mutate(arr_rate = (arrests_civreport/pop_1019)*1000)

# have a peek
ggplot(arr_alc, aes(x = year, y = arr_rate, color = locality)) +
  geom_line()


# Save data ----
write_csv(arr_alc, "data/arrests_alcoholrelated.csv")
