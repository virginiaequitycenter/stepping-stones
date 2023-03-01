# Stepping Stones Data: Arrests, Crimes against Persons ----
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
# 2021-2018
# Charlottesville and Albemarle Tables: NIBRS Agency Crime Overview
# sum crimes against persons
# Statewide Tables: Number of Arrestees by Offense & Age Range
# sum crimes against persons

# 2017 -1999
# Charlottesville and Ablemarle: Arrest Totals By County, City and Other
# sum murder, negligent manslaughter, kidnapping, sex offenses, aggravated assault, simple assault/intimidation
# Statewide Tables: Juvenile Arrests-Offenses by Age
# sum Murder/Nonnegligent Manslaughter, Negligent Manslaughter, Kidnapping/Abduction, Forcible Rape, Forcible Sodomy, Sexual Assault with an Object, Forcible Fondling, Aggravated Assault, Simple Assault, Intimidation

# Libraries ----
library(tidyverse)

# Read in csv file ----
arr_cap <- read_csv("datadownloads/arrests_crimes_against_persons.csv")

arr_cap <- arr_cap %>% 
  mutate(locality = ifelse(locality == "Charlotteville", "Charlottesville", locality),
         fips = case_when(
           locality == "Albemarle" ~ "51003",
           locality == "Charlottesville" ~ "51540",
           locality == "Virginia" ~ "51"
           ))

# Read in population data ----
pop <- read_csv("data/pop_data_cdc.csv")

pop <- pop %>% 
  select(fips, year, pop_1017) %>% 
  mutate(fips = as.character(fips))

# join data frames
arr_cap <- left_join(arr_cap, pop)

# fill in pop data for 2021 and generate rate
arr_cap <- arr_cap %>% 
  group_by(locality) %>% 
  fill(pop_1017, .direction = "up") %>% 
  ungroup()

# create rate per 1000
arr_cap <- arr_cap %>% 
  mutate(arr_rate = (arrests_civreport/pop_1017)*1000)

# have a peek
ggplot(arr_cap, aes(x = year, y = arr_rate, color = locality)) +
  geom_line()


# Save data ----
write_csv(arr_cap, "data/arrests_crimesagainstpersons.csv")
