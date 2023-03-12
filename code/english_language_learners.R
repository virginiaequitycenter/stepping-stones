# Stepping Stones Data: SOL Pass Rates ----
# Updated 2023-01-19
# Contributor: Michele Claibourn, Ramya, Kate, Michael
# Generate locality and state counts/percents of english language learners in schools

# From VDOE: https://p1pe.doe.virginia.gov/buildatable/fallmembership
# 2004-2023
# For Charlottesville, Albemarle
#   School Years: select all
#   Report Level: division
#   Divisions: Albemarle and Charlottesville
#   Race: All races
#   Gender: All genders
#   Grades: All grades
#   Self-reporting categories: All students for all except 
#   English Learners: Yes and No
#   Include Former ELs: No (don't check)
# For State
#   Change Report Level: state
#
# Proposed Citation: 
# Virginia Department of Education, Fall Membership Build-A-Table, 2004-2023.
# https://p1pe.doe.virginia.gov/buildatable/fallmembership
# accessed March 6, 2023


# ..................................................
# Load libraries ---
library(tidyverse)
library (janitor)
#library(ggthemes)

# ..................................................
# Reading in data ---
ell_cvlalb <- read_csv("datadownloads/ell_cvlalb_fallmembership.csv") %>% 
  clean_names()
ell_state <- read_csv("datadownloads/ell_state_fallmembership.csv") %>% 
  clean_names() 

## Bind division and state ----
ell <- ell_state %>% 
  mutate(division_name = "Virginia", division_number = 0) %>% 
  bind_rows(ell_cvlalb)

# ..................................................
# Prep data ---
ell <- ell %>% 
  # sum studens (Y and N) by division/year
  group_by(division_name, division_number, school_year) %>% 
  mutate (total_count = str_remove(total_count,","),
          total_count = ifelse(total_count == "<", "0", total_count),
          total_count = as.numeric(total_count),
          student_count = sum(total_count)) %>% 
  # keep only english leaners and create percent
  ungroup() %>% 
  filter(english_learners == "Y") %>% 
  mutate(pct_english_learners = (total_count/student_count)*100) %>% 
  # keep select colums and rename some
  select(school_year, division_name, division_number,
         english_learners = total_count, student_count, pct_english_learners)

# Shorten division names and add numeric school year (spring year)
ell <- ell %>% 
  mutate(locality = str_remove(division_name, " County| City"),
         year = str_sub(school_year, 6,9),
         year = as.numeric(year))

# have a peek 

ggplot(ell, aes(x = school_year, y = pct_english_learners, color = locality, group = locality)) +
  geom_line() + geom_point()

# ..................................................
# Save data ----
write.csv(ell, "data/english_language_learners.csv")

