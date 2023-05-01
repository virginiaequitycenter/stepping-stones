# Stepping Stones Data: Weapons Offenses in Schools ----
# Updated 2023-03-09
# Contributor: Michele Claibourn
# Acquire data from Discipline, Crime, and Violence Reports
# https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Education, "Discipline, Crime, and Violence Reports" 2006-2021.
# https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education


# libraries ----
library(tidyverse)
library(readxl)
library(janitor)

# Excel files from DCV reports ----
# 2018-2021 
## create urls vector ----
urls <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/20785/638043641312530000", # 2018
  "https://www.doe.virginia.gov/home/showpublisheddocument/20791/638043641330800000", # 2019
  "https://www.doe.virginia.gov/home/showpublisheddocument/20797/638043641350800000", # 2020
  "https://www.doe.virginia.gov/home/showpublisheddocument/20803/638043641373770000"  # 2021
)

## create vector of destination file names ----
dest <- paste0("datadownloads/dcv/dcv_", c("2018", "2019", "2020", "2021"), ".xlsx")

## download files ----
if (!dir.exists(here("datadownloads/dcv"))) 
{dir.create(here("datadownloads/dcv"))}

walk2(urls, dest, download.file, method = "curl", extra = "-k")


## read in files ----
# test <- read_excel(dest[1], sheet = 1)
dcv_data <- map(dest, ~read_excel(.x, sheet = 1))
dcv_data <- bind_rows(dcv_data) %>% clean_names()


## filter and prep ----
# find relevant offense categories
dcv_data %>%
  group_by(offense_category_description, offense_category) %>% 
  summarize(n = n()) %>% 
  view()
# Choosing: Assault/Battery, Fighting/Conflict, Kidnapping, Robbery/Person/Force or Threat of Force,
#   Sexual Offenses, Threats/Verbal/Physical (removed weapons, given separate weapons variable)
# This set is trying to replicate the categories on the city's spreadsheet
# but I'm not sure these would be the categories I'd select from the start...

dcv_cvlalb <- dcv_data %>% 
  filter(division_number %in% c(2,104),
         offense_category %in% c(3,12,18,20,22,24)) %>% 
  mutate(division_name = ifelse(division_name == "Albemarle County", "Albemarle", "Charlottesville"))

# sum incidents by division, year
dcv_sum <- dcv_cvlalb %>% 
  group_by(division_number, division_name, school_year) %>% 
  summarize(count = sum(count_of_incidents))     
# wow, the albemarle ones seem really high compared to city's spreadsheet

## create state totals ----
dcv_va <- dcv_data %>% 
  filter(offense_category %in% c(3,12,18,20,22,24)) %>% 
  group_by(school_year) %>% 
  summarize(count = sum(count_of_incidents)) %>% 
  mutate(division_name = "Virginia", division_number = 0)

## bind state and localities ----
dcv1821 <- bind_rows(dcv_sum, dcv_va)

### create (single) year
dcv1821 <- dcv1821 %>% 
  mutate(year = as.numeric(str_sub(school_year, 6,9)))


# Pull in student count data ----
# Downloaded previously, team 1
students <- read_csv("datadownloads/fall_membership_statistics_team1.csv") %>% 
  clean_names()

# locality totals
stud_cvlalb <- students %>% 
  filter(division_name %in% c("Albemarle County", "Charlottesville City")) %>% 
  select(school_year, division = division_name, students = total_count) %>% 
  mutate(division = ifelse(division == "Albemarle County", "Albemarle", "Charlottesville"))

# state totals
stud_va <- students %>% 
  group_by(school_year) %>% 
  summarize(students = sum(total_count)) %>% 
  mutate(division = "Virginia")

# bind locality and state totals together
students <- bind_rows(stud_cvlalb, stud_va) %>% 
  rename(division_name = division)


# Join weapons offense and student counts ----
df_students <- dcv1821 %>% 
  left_join(students)

# and generate the rate 
df_students <- df_students %>% 
  mutate(rate = (count/students)*1000)

# have a peek
ggplot(df_students, aes(x = year, y = rate, color = division_name, group = division_name)) + geom_line()


# save file to date ----
write_csv(df_students, "data/school_physical_violence.csv")
# df_students <- read_csv("data/school_physical_violence.csv")