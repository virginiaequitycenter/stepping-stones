# Stepping Stones Data: Special Education Services ----
# Updated 2023-02-09
# Contributor: Michele Claibourn
# Generate locality and state students receiving 
#   special education services

# Numerator - children with disabilities, receiving special education services
# From VDOE: https://p1pe.doe.virginia.gov/buildatable/dec1
# 2011-2022
# For Charlottesville, Albemarle
#   School Years: select all
#   Report Level: division
#   Divisions: Albemarle and Charlottesville
#   Race: All races
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
sped_state <- read_csv("datadownloads/dec_1_statistics_va.csv") %>% 
  clean_names() %>% 
  rename(total_sped = total_count)
enroll_state <- read_csv("datadownloads/fall_membership_statistics_va.csv") %>% 
  clean_names() %>% 
  select(-c(part_time_count_all_grades))
sped_albcvl <- read_csv("datadownloads/dec_1_statistics_albcvl.csv") %>% 
  clean_names() %>% 
  rename(total_sped = total_count)
enroll_albcvl <- read_csv("datadownloads/fall_membership_statistics_albcvl.csv") %>% 
  clean_names() %>% 
  select(-c(part_time_count_all_grades))

# Join and bind data ----
# add division_name and division_number to state (for consistency)
state <- left_join(sped_state, enroll_state) %>% 
  mutate(division_number = 0,
         division_name = "Virginia")
albcvl <- left_join(sped_albcvl, enroll_albcvl)

sped <- bind_rows(albcvl, state) 

# Create percent ----
sped <- sped %>% 
  mutate(percent_sped = (total_sped/full_time_count_all_grades)*100) %>% 
  rename(total_fulltime = full_time_count_all_grades)

# And have a peek
sped %>% 
  ggplot(aes(x = school_year, y = percent_sped, 
             color = division_name, group = division_name)) +
  geom_line()


# Save data ----
write_csv(sped, "data/sped_services.csv")
