# Stepping Stones: Free-Reduced (Economically Disadvantaged) Students
# Finance-Children Qualifying for Free Reduced Meals (% of students)
# Team 1 Fara Islam, Lekha Mereddy, & Lily Slonim
# Edited to pull by race by Lee LeBoeuf
# Last Updated: May 26, 2023 
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
#  - Race: Disaggregated  
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
## Total student counts 
# Albemarle and Charlottesville
cvilleAL_totalstudents <- read.csv("fall_membership_statistics_AlbeCville.csv") 
cvilleAL_totalstudents <- clean_names(cvilleAL_totalstudents)

# All of VA
va_totalstudents <- read.csv("fall_membership_statistics_AllVA.csv") 
va_totalstudents <- clean_names(va_totalstudents)
va_totalstudents$division_number <- "51"
va_totalstudents$division_name <- "Virginia"

# binding everything together 
totaldat <- rbind(cvilleAL_totalstudents, va_totalstudents)

totaldat <- totaldat %>%
  rename(totalstudents = total_count) %>%
  select(-full_time_count_all_grades, -part_time_count_all_grades)

## Economically disadvantaged counts
# All of Virginia
va_econdis <- read_csv("economically_disadvantaged-AllVA.csv")
va_econdis <- clean_names(va_econdis)
va_econdis$division_number <- "51"
va_econdis$division_name <- "Virginia"

# By Division
cvilleAL_econdis <- read_csv("economically_disadvantaged-Division.csv")
cvilleAL_econdis <- clean_names(cvilleAL_econdis)

econdisddat <- rbind(va_econdis, cvilleAL_econdis)

econdisddat <- econdisddat %>%
  rename(econdis_students = total_count) %>%
  select(-disadvantaged, -full_time_count_all_grades, -part_time_count_all_grades)

# Merging the number of total students and the number of economically disadvantaged students 

alldat <- merge(totaldat, econdisddat, by = c("school_year", "race", "division_name", "division_number"))

### Percentage of Economically Disadvantaged Students 
# Have to remove the weird character signifying small numbers of students 
alldat$econdis_students <- ifelse(alldat$econdis_students == "<", NA, alldat$econdis_students)
alldat$econdis_students <- stringi::stri_replace_all_fixed(alldat$econdis_students, ",", "")
alldat$econdis_students <- as.numeric(alldat$econdis_students)

alldat$totalstudents <- ifelse(alldat$totalstudents == "<", NA, alldat$totalstudents)
alldat$totalstudents <- stringi::stri_replace_all_fixed(alldat$totalstudents, ",", "")
alldat$totalstudents <- as.numeric(alldat$totalstudents)

# Calculation 
alldat$percent <- round(alldat$econdis_students / alldat$totalstudents * 100, 2)

# Save Data ----

write_csv(alldat,"econ_disad_students_Race.csv")


