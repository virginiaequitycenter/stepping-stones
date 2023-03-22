# Stepping Stones Data: Population/Age ----
# Updated 2023-01-13
# Contributor: Michele Claibourn
# Generate locality and state population estimates by age range and totals
#.  used to create percents or rates for multiple metrics

# From CDC Wonder: https://wonder.cdc.gov/wonder/help/bridged-race.html
# 1990-2020 Request
# For Charlottesville, Albemarle
#.  Group Results by: County, Yearly, Age
#.  Select: States; Virginia - Albemarle, Charlottesville
#.  Select: all ages; all years
#   Send and Export
# For State
#.  Group Results by: Yearly, Age
#.  Select: States; Virginia
#.  Select: all ages; all years
#   Send and Export
#
# Proposed Citation from page: 
# U.S. Department of Health and Human Services, CDC Wonder, "Bridged-Race Population Estimates, United States July 1st resident population by state, county, age, sex, bridged-race, and Hispanic origin." 1990-2020 database (Vintage 2020).
# https://wonder.cdc.gov/wonder/help/bridged-race.html
# 
# Note: For 2008-2019, can use ACS 5-year estimates instead (B01001)
#.   e.g., 2006-2010 for 2008, 2007-2011 for 2009, etc.
#    alternatively, single-age estimates are also provided by SEER
#    https://seer.cancer.gov/popdata/download.html


# Libraries ----
library(tidyverse)
library(janitor)

# Read in data ----
cdc_state <- read_tsv("datadownloads/VA VAP Historical-state.txt")
cdc_albcvl <- read_tsv("datadownloads/VA VAP Historical-cvlalb.txt")

# Generate charlottesville and albemarle population numbers ----
cdc_albcvl <- cdc_albcvl %>% 
  clean_names()

pop_tot <- cdc_albcvl %>% 
  filter(notes == "Total", !is.na(yearly_july_1st_estimates)) %>% 
  select(fips = county_code, year = yearly_july_1st_estimates, 
         pop_tot = population)

pop_18over <- cdc_albcvl %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code >= 18 ) %>% 
  group_by(county_code, yearly_july_1st_estimates) %>% 
  summarize(pop_18over = sum(population)) %>% 
  select(fips = county_code, year = yearly_july_1st_estimates, 
         pop_18over)

pop_17under <- cdc_albcvl %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code < 18 ) %>% 
  group_by(county_code, yearly_july_1st_estimates) %>% 
  summarize(pop_17under = sum(population)) %>% 
  select(fips = county_code, year = yearly_july_1st_estimates, 
         pop_17under)

pop_1017 <- cdc_albcvl %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code > 9 & age_code < 18 ) %>% 
  group_by(county_code, yearly_july_1st_estimates) %>% 
  summarize(pop_1017 = sum(population)) %>% 
  select(fips = county_code, year = yearly_july_1st_estimates, 
         pop_1017)

pop_1019 <- cdc_albcvl %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code > 9 & age_code < 20 ) %>% 
  group_by(county_code, yearly_july_1st_estimates) %>% 
  summarize(pop_1019 = sum(population)) %>% 
  select(fips = county_code, year = yearly_july_1st_estimates, 
         pop_1019)

pop_cvlalb <- left_join(pop_tot, pop_18over) %>% 
  left_join(pop_17under) %>% 
  left_join(pop_1017) %>% 
  left_join(pop_1019)


# Generate state population numbers ----
cdc_state <- cdc_state %>% 
  clean_names()

pop_tot <- cdc_state %>% 
  filter(notes == "Total", !is.na(yearly_july_1st_estimates)) %>% 
  mutate(fips = 51) %>% 
  select(fips, year = yearly_july_1st_estimates, pop_tot = population)

pop_18over <- cdc_state %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code >= 18 ) %>% 
  group_by(yearly_july_1st_estimates) %>% 
  summarize(pop_18over = sum(population)) %>% 
  mutate(fips = 51) %>% 
  select(fips, year = yearly_july_1st_estimates, pop_18over)

pop_17under <- cdc_state %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code < 18 ) %>% 
  group_by(yearly_july_1st_estimates) %>% 
  summarize(pop_17under = sum(population)) %>% 
  mutate(fips = 51) %>% 
  select(fips, year = yearly_july_1st_estimates, pop_17under)

pop_1017 <- cdc_state %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code > 9 & age_code < 18 ) %>% 
  group_by(yearly_july_1st_estimates) %>% 
  summarize(pop_1017 = sum(population)) %>% 
  mutate(fips = 51) %>% 
  select(fips, year = yearly_july_1st_estimates, pop_1017)

pop_1019 <- cdc_state %>% 
  filter(is.na(notes)) %>% 
  mutate(age_code = as.numeric(age_code)) %>% 
  filter(age_code > 9 & age_code < 20 ) %>% 
  group_by(yearly_july_1st_estimates) %>% 
  summarize(pop_1019 = sum(population)) %>% 
  mutate(fips = 51) %>% 
  select(fips, year = yearly_july_1st_estimates, pop_1019)

pop_va <- left_join(pop_tot, pop_18over) %>% 
  left_join(pop_17under) %>% 
  left_join(pop_1017) %>% 
  left_join(pop_1019)


# Combine and save ----
pop_data <- bind_rows(pop_cvlalb, pop_va)

write_csv(pop_data, "data/pop_data_cdc.csv")


# Child Population by Race ----

# From CDC Wonder: https://wonder.cdc.gov/wonder/help/bridged-race.html
# 1990-2020 Request
# For Charlottesville, Albemarle
#.  Group Results by: County, Yearly, Race, Age
#.  Select: States; Virginia - Albemarle, Charlottesville
#.  Select: ages <1 through 17; all years, all races, all ethnicities, all genders
#   Send and Export
# For State
#.  Group Results by: Yearly, Race, Age
#.  Select: States; Virginia
#.  Select: ages <1 through 17; all years, all races, all ethnicities, all genders
#   Send and Export

cdc_state <- read_tsv("datadownloads/Bridged-Race Population Estimates 1990-2020_virginia.txt")
cdc_albcvl <- read_tsv("datadownloads/Bridged-Race Population Estimates 1990-2020_cvlalb.txt")

# Generate charlottesville and albemarle population numbers ----
cdc_albcvl <- cdc_albcvl %>% 
  clean_names()

pop_17under_race_cvlalb <- cdc_albcvl %>% 
  filter(is.na(notes)) %>% 
  group_by(county, county_code, yearly_july_1st_estimates, race) %>% 
  summarize(pop_17under = sum(population)) %>% 
  select(locality = county, fips = county_code, year = yearly_july_1st_estimates, 
         race, pop_17under)

# Generate state population numbers ----
cdc_state <- cdc_state %>% 
  clean_names()

pop_17under_race_va <- cdc_state %>% 
  filter(is.na(notes)) %>% 
  group_by(yearly_july_1st_estimates, race) %>% 
  summarize(pop_17under = sum(population)) %>% 
  mutate(fips = 51, locality = "Virginia") %>% 
  select(locality, fips, year = yearly_july_1st_estimates, race, pop_17under)

# Combine and save ----
pop_17under_race <- bind_rows(pop_17under_race_cvlalb, pop_17under_race_va)

write_csv(pop_17under_race, "data/pop_17under_race_data_cdc.csv")
