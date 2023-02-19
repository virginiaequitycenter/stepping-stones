# Stepping Stones: Free-Reduced (Economically Disadvantaged) Students
# Finance-Children Qualifying for Free Reduced Meals (% of students)
# Team 1 Fara Islam, Lekha Mereddy, & Lily Slonim
# Last Updated: Feburary 15, 2023 
# 
# Based on Virginia Department of Education Economically Disadvantaged Estimates
# https://p1pe.doe.virginia.gov/buildatable/fallmembership
# Charlottesville, Albemarle, State
#
# Proposed Citation:
# Virginia Department of Education, "Fall Membership Build-A-Table." 2004-2023.
# https://p1pe.doe.virginia.gov/buildatable/fallmembership
#
#
# Data was downloaded from Virginia Department of Education 
# (https://p1pe.doe.virginia.gov/buildatable/fallmembership)
# The Economically Disadvantaged Data Set used the following fields: 
#  - School Years: 2004 - 2023 
#  - Report Level: Division (change to State for state wide estimates)
#  - Race: All 
#  - Gender: All 
#  - Grade: All
#  - Disadvantaged: Yes (change to All for totals)
#  - Foster Care: All
#  - Homeless: All
#  - Migrant: All
#  - Military Connected: All
#  - English Learners: All
#  - Disabled: All
# A student is flagged as disadvantaged if they meet one of the categories below 
# (source: https://www.scorecard.doe.virginia.gov/statistics_reports/research_data/data_elements.shtml) 
# 1) is eligible for Free/Reduced Meals
# 2) receives TANF
# 3) is eligible for Medicaid
# 4) identified as either Migrant or experiencing Homelessness 
# However, the Free/Reduced lunch eligibility is the broadest (highest threshold) 
# criteria (source: https://data.mundidev.com/data-use/data-use-understanding-data/data-use-understanding-data-important-terms/)

# ..................................................

# Load libraries ----
library(tidyverse)
library(janitor)
library(readxl)

# ..................................................

#  Load Data  ----
# disadvantaged counts (from Report Level = Division; Disadvantaged = Yes)
va_economically_disadvantaged <- read_csv("datadownloads/fall_membership_disadvantaged_2004-2023.csv") 
va_economically_disadvantaged<- clean_names(va_economically_disadvantaged)

# total student counts (from Report Level = Division; Disadvantaged = All)
va_total_students <- read_csv("datadownloads/fall_membership_statistics_team1.csv")
va_total_students <- clean_names(va_total_students)


# ..................................................
# Percentage of Economically Disadvantaged Students  ----

## Charlottesville & Albemarle County ----

# Filter for Charlottesville & Albemarle County 
economically_disadvantaged_cville_albe <- va_economically_disadvantaged %>% 
  filter(division_name == "Charlottesville City"| division_name == "Albemarle County" )  

total_students_cville_albe  <- va_total_students %>% 
  filter(division_name == "Charlottesville City"| division_name == "Albemarle County" )  

# Rename variables
total_students_cville_albe <- total_students_cville_albe  %>%
  rename(fulltime_total = full_time_count_all_grades,
         parttime_total = part_time_count_all_grades,
         totstudents = total_count)

# Join total pop to economically disadvantaged data set 
economically_disadvantaged_cville_albe <- economically_disadvantaged_cville_albe  %>% 
  left_join(total_students_cville_albe)

# Calculate the Percentage of Students who are economically disadvantaged 
economically_disadvantaged_cville_albe <- economically_disadvantaged_cville_albe %>% 
  mutate(percent = (total_count/totstudents)*100)


## Virginia ----

# Calculate total population of Students
va_total_students_2 <- va_total_students   %>% 
  group_by(school_year) %>% 
  summarize(totstudents = sum(total_count)) %>% 
  ungroup()

# Calculate total number of economically disadvantaged students in Virginia by year 
va_economically_disadvantaged_2 <- va_economically_disadvantaged %>% 
  select(school_year, 
         disadvantaged, 
         full_time_count_all_grades, 
         part_time_count_all_grades, 
         total_count) %>% 
  group_by(school_year) %>% 
  summarize(economically_disadvantaged_students = sum(total_count)) %>% 
    ungroup()

# Join total pop to economically disadvantaged data set 
va_economically_disadvantaged_2 <- va_economically_disadvantaged_2   %>% 
  left_join(va_total_students_2)

# Calculate the Percentage of Students who are economically disadvantaged 
va_economically_disadvantaged_2 <- va_economically_disadvantaged_2 %>% 
  mutate(percent = (economically_disadvantaged_students/totstudents)*100)


# Join locality and state data ----
# reduce to common variables
economically_disadvantaged_cville_albe <- economically_disadvantaged_cville_albe %>% 
  select(school_year, 
         division_number, 
         division_name, 
         economically_disadvantaged_students = total_count, 
         totstudents, 
         percent)

# Add column names not present in the VA data (division_number, division_name)
va_economically_disadvantaged_2 <- va_economically_disadvantaged_2 %>% 
  mutate(division_number = 0, division_name = "Virginia")

# Bind by rows
economically_disadvantaged <- bind_rows(economically_disadvantaged_cville_albe,
                                        va_economically_disadvantaged_2)

# have a peek
ggplot(economically_disadvantaged, aes(x = school_year, y = percent, 
                                       color = division_name, group = division_name)) +
  geom_line()


# Save Data ----

write_csv(economically_disadvantaged,"data/economically_disadvantaged_students.csv")


