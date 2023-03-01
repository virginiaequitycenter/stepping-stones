# Stepping Stones
# Reports of physical violence in schools
# Based on Discipline, Crime, and Violence Reports, 2018-2021

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
#   Sexual Offenses, Threats/Verbal/Physical, Weapons
# This set is trying to replicate the categories on the city's spreadsheet
# but I'm not sure these would be the categories I'd select from the start...

dcv_cvlalb <- dcv_data %>% 
  filter(division_number %in% c(2,104),
         offense_category %in% c(3,12,18,20,22,24,29)) %>% 
  mutate(division_name = ifelse(division_name == "Albemarle County", "Albemarle", "Charlottesville"))

# sum incidents by division, year
dcv_sum <- dcv_cvlalb %>% 
  group_by(division_number, division_name, school_year) %>% 
  summarize(count = sum(count_of_incidents))     
# wow, the albemarle ones seem really high compared to city's spreadsheet

## create state totals ----
dcv_va <- dcv_data %>% 
  filter(offense_category %in% c(3,12,18,20,22,24,29)) %>% 
  group_by(school_year) %>% 
  summarize(count = sum(count_of_incidents)) %>% 
  mutate(division_name = "Virginia", division_number = 0)

## bind state and localities ----
dcv1821 <- bind_rows(dcv_sum, dcv_va)

# save file to date ----
write_csv(dcv1821, "data/school_physical_violence.csv")
