# Stepping Stones Data: SOL Pass Rates ----
# Updated 2023-01-19
# Contributor: Michele Claibourn
# Generate locality and state SOL pass rates
#   grades 3,5,8; english and math

# From VDOE: https://p1pe.doe.virginia.gov/buildatable/testresults
# 2006-2021
# For Charlottesville, Albemarle
#   School Years: select all
#   Report Level: division
#   Divisions: Albemarle and Charlottesville
#   Race: All races
#   Gender: All genders
#   Grades: All grades
#   Self-reporting categories: All students for all 
#   Test Level: Grades 3, 5, 8
#   Test Source: SOL
#   Subject Area: English Reading, Mathematics
#   Tests: All tests
#   Statistic: Total count, Pass count, Pass rate
# For State
#   Change Report Level: state
#
# Proposed Citation: 
# Virginia Department of Education, Test Results Build-A-Table, 2006-2021.
# https://p1pe.doe.virginia.gov/buildatable/testresults
# accessed January 19, 2023


# Libraries ----
library(tidyverse)
library(janitor)

# Read in data ----
sol_state <- read_csv("datadownloads/vdoe_sol_virginia.csv") %>% 
  clean_names()
sol_albcvl <- read_csv("datadownloads/vdoe_sols_cville_albemarle.csv") %>% 
  clean_names()

# Bind data ---
# combine local and state data
# add division_name and division_number to state (for consistency)
sol_state <- sol_state %>% 
  mutate(division_number = 0,
         division_name = "Virginia")

sols <- bind_rows(sol_albcvl, sol_state)

# Go ahead and remove a few variables that won't be used
# and make grade a number (for easier filtering)
sols <- sols %>% 
  mutate(grade = str_extract(test_level, "[0-9]")) %>% 
  select(-c(division_number, test_source, test_level))

# And have a peek
sols %>% 
  filter(subject == "Mathematics" & grade == 3) %>% 
  ggplot(aes(x = school_year, y = pass_rate, 
                 color = division_name, group = division_name)) +
  geom_line()


# Save data ----
write_csv(sols, "data/sol_pass.csv")
