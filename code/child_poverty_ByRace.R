# Stepping Stones Data: Child Poverty ----
# Updated 2023-06-05
# Contributor: Michele Claibourn & Lee LeBoeuf
# Generate child poverty estimates
#
# Based on Census Small Area Income and Poverty Estimates
# https://www.census.gov/programs-surveys/saipe/data/datasets.html
# Charlottesville, Albemarle, State
#
# Choose State and County Estimates
# Copy link for Virginia: 
# https://www2.census.gov/programs-surveys/saipe/datasets/2021/2021-state-and-county/est21-va.txt
# 
# Proposed Citation: 
# U.S. Census Bureau, "SAIPE State and County Estimates for 2021."  
# https://www.census.gov/programs-surveys/saipe/data/datasets.html

# Child poverty variables (Combining under 5, 5 years, 6-11, 12-14, 15 years, and 16-17)
# White alone male: B17001A_004, B17001A_005, B17001A_006, B17001A_007, B17001A_008, B17001A_009
# White alone female: B17001A_018, B17001A_019, B17001A_020, B17001A_021, B17001A_022, B17001A_023
# Black alone male: B17001B_004, B17001B_005, B17001B_006, B17001B_007, B17001B_008, B17001B_009
# Black alone female: B17001B_018, B17001B_019, B17001B_020, B17001B_021, B17001B_022, B17001B_023
# AIAN alone male: B17001C_004, B17001C_005, B17001C_006, B17001C_007, B17001C_008, B17001C_009
# AIAN alone female: B17001C_018, B17001C_019, B17001C_020, B17001C_021, B17001C_022, B17001C_023
# Asian alone male: B17001D_004, B17001D_005, B17001D_006, B17001D_007, B17001D_008, B17001D_009
# Asian alone female: B17001D_018, B17001D_019, B17001D_020, B17001D_021, B17001D_022, B17001D_023
# Native Hawaiian alone male: B17001E_004, B17001E_005, B17001E_006, B17001E_007, B17001E_008, B17001E_009
# Native Hawaiian alone female: B17001E_018, B17001E_019, B17001E_020, B17001E_021, B17001E_022, B17001E_023
# Some other race male: B17001F_004, B17001F_005, B17001F_006, B17001F_007, B17001F_008, B17001F_009
# Some other race female: B17001F_018, B17001F_019, B17001F_020, B17001F_021, B17001F_022, B17001F_023
# Multi male: B17001G_004, B17001G_005, B17001G_006, B17001G_007, B17001G_008, B17001G_009
# Multi female: B17001G_018, B17001G_019, B17001G_020, B17001G_021, B17001G_022, B17001G_023
# Hispanic male: B17001I_004, B17001I_005, B17001I_006, B17001I_007, B17001I_008, B17001I_009
# Hispanic female: B17001I_018, B17001I_019, B17001I_020, B17001I_021, B17001I_022, B17001I_023


# Libraries ----
library(tidyverse)
library(tidycensus)
library(stringr)

# rename function 
rename <- function(data_names,originals,replacements){
  for(i in 1:length(originals)){
    data_names <- stringi::stri_replace_all_fixed(data_names, originals[i],replacements[i])
  }
  return(data_names)
}

# read in variable list
varstable <- read.csv("ChildPovertyVariables.csv")

# read in population data 
pop <- read.csv("child_pop_race_ethn.csv")

# Get data ----
# ..................................................
# Pull variables, Charlottesville/Albemarle and VA----
years <- 2010:2021

# poverty variables 
cpovcounty <- map_dfr(years,
                      ~get_acs(
                        geography = "county",
                        state = "51",
                        county = c("003", "540"),
                        year = .x,
                        survey = "acs5",
                        var = varstable$OldName,
                        output = "tidy") %>%
                        mutate(year = .x))

cpovstate <- map_dfr(years,
                     ~get_acs(
                       geography = "state",
                       state = "51",
                       year = .x,
                       survey = "acs5",
                       var = varstable$OldName,
                       output = "tidy") %>%
                       mutate(year = .x))


# Prep data ----
# ..................................................
# Renaming variables 

cpovcounty$variable <- rename(cpovcounty$variable,varstable$OldName,varstable$NewName)
cpovcounty[c("race", "GenderAge")] <- stringr::str_split_fixed(cpovcounty$variable, "_", 2)

cpovstate$variable <- rename(cpovstate$variable,varstable$OldName,varstable$NewName)
cpovstate[c("race", "GenderAge")] <- stringr::str_split_fixed(cpovstate$variable, "_", 2)

# Adding columns of the same race and 
cpovcounty <- cpovcounty %>%
  group_by(race, year, GEOID) %>%
  summarise(estimate = sum(estimate))

cpovstate <- cpovstate %>%
  group_by(race, year, GEOID) %>%
  summarise(estimate = sum(estimate))

# binding states and localities together 
cpov <- rbind(cpovcounty, cpovstate)

cpov <- cpov %>%
  dplyr::rename(pov_count = estimate)

cpov <- merge(cpov, pop, by = c("GEOID", "year", 'race'), all.x = T)

cpov <- cpov %>%
  mutate(pov_percent = round(pov_count / pop_count * 100, 2)) %>%
  select(-ethn, -pop_percent)

cpov$NAME <- ifelse(cpov$GEOID == "51", "Virginia", 
                    ifelse(cpov$GEOID == "51003", "Albemarle County, Virginia", 
                           ifelse(cpov$GEOID == "51540", "Charlottesville city, Virginia", cpov$GEOID)))

# Save data ----
write_csv(cpov, "child_pov_byRace.csv")
