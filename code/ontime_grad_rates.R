# Stepping Stones Data: On-time Graduation Rates ----
# Updated 2023-02-09
# Authors: Jayla Hart, Somin Lee, Charlie Bruce, Michele Claibourn
# Generate locality and state on-time graduation

# From VDOE: https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
# 2008-2022
# For Charlottesville, Albemarle
#   School Years: select all
#   Report Level: division
#   Divisions: Albemarle and Charlottesville
#   Race: All races
#   Gender: All genders
#   Disadvantaged: All students 
#   Foster Care: All students
#   Homeless: All students
#   Military Connected: All students
#   English Learning: All students
#   Disability Type: All students
#   Type of Graduation Rate: Virginia on-time graduation rate
#   Number of Years After First Entering 9th Grade: 4 years
#   Statistic: Graduation rate, Total graduates, Students in cohort
# For State:
#   Change Report Level: state

# Proposed Citation: 
# Virginia Department of Education,  Cohort Graduation Build-A-Table, 2008-2022.
# https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
# accessed January 19, 2023

# Libraries ----
library(tidyverse)
library(janitor)

# Read in data ----
grad_state <- read_csv("datadownloads/graduation_statistics_va.csv") %>% 
  clean_names() %>% 
  mutate(division_number = 0,
         division_name = "Virginia")
grad_albcvl <- read_csv("datadownloads/graduation_statistics_albcvl.csv") %>% 
  clean_names() 

# Join and bind data ----
grad <- bind_rows(grad_albcvl, grad_state)

grad <- grad %>% 
  mutate(graduation_rate = str_remove(graduation_rate, "%"),
         graduation_rate = as.numeric(graduation_rate))

# And have a peek
grad %>% 
  ggplot(aes(x = cohort_year, y = graduation_rate, 
             color = division_name, group = division_name)) +
  geom_line()


# Save data ----
write_csv(grad, "data/graduation_rates.csv")
