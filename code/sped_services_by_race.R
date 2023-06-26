# Stepping Stones Data: Special Education Services ----
# Updated 2023-02-09
# Contributor: Michele Claibourn and Lee LeBoeuf
# Generate locality and state students receiving 
#   special education services

# Numerator - children with disabilities, receiving special education services
# From VDOE: https://p1pe.doe.virginia.gov/buildatable/dec1
# 2011-2022
# For Charlottesville, Albemarle
#   School Years: select all
#   Report Level: division
#   Divisions: Albemarle and Charlottesville
#   Race: Highlight all races individually instead of selecting "All students"
#   Gender: All genders
#   Placement: All placements
#   Age Group: All ages
#   Grade: All grades
#   Disadvantaged: All students 
#   Foster Care: All students
#   Homeless: All students
#   Migrant: All students
#   Military Connected: All students
#   English Learning: All students
#   Disability Type: All students
# For State:
#   Change Report Level: state
#
# Denominator - fall enrollment
# From VDOE: https://p1pe.doe.virginia.gov/buildatable/fallmembership

# Proposed Citation: 
# Virginia Department of Education, December 1 Build-A-Table, 2011-2022.
# https://p1pe.doe.virginia.gov/buildatable/dec1
# accessed January 19, 2023


# Libraries ----
library(tidyverse)
library(janitor)

# Read in data ----
state <- read.csv("state_statistics.csv") %>% 
  clean_names() %>% 
  rename(total_sped = total_count)

division <- read.csv("division_statistics.csv") %>% 
  clean_names() %>% 
  rename(total_sped = total_count)

# fall membership for total enrollment by race
totalenrollstate <- read.csv("fall_membership_statistics_state.csv") %>%
  rename(school_year = "School.Year",
         full_time_count = "Full.Time.Count..All.Grades.", 
         part_time_count = "Part.Time.Count..All.Grades.",
         total_count = "Total.Count",
         race = "Race") %>% 
  mutate(division_number = 0,
         division_name = "Virginia")

totalenrolldiv <- read.csv("fall_membership_statistics_div.csv") %>%
  rename(school_year = "School.Year",
         full_time_count = "Full.Time.Count..All.Grades.", 
         part_time_count = "Part.Time.Count..All.Grades.",
         total_count = "Total.Count",
         division_number = "Division.Number",
         division_name = "Division.Name",
         race = "Race") 

totalenrollall <- bind_rows(totalenrollstate, totalenrolldiv) 

# Join and bind data ----
# add division_name and division_number to state (for consistency)
state <- state %>% 
  mutate(division_number = 0,
         division_name = "Virginia")

sped <- bind_rows(state, division) 

sped <- merge(sped, totalenrollall, by = c("school_year", "race", "division_number", "division_name"))

sped$total_sped <- ifelse(sped$total_sped == "<", NA, sped$total_sped)

write.csv(sped, "sped_services_by_race.csv")






