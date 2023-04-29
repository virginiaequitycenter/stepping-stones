# Title: Stepping Stones Project
# Point-in-Time Count of People Experiencing Homelessness
# Michele Claibourn
# Team 1: Connor Eads, Maria Morrissey, Vani Agarwal, Melinda Wong
# Last Updated: 2023-03-18

# Proposed Citation
# Blue Ridge Area Coalition for the Homeless, "Point-in-Time Count", 2009-2022.

# ..................................................
# Load Libraries, Read in Data, & Prep Variables ----

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

## Read data
pit <- read_excel("datadownloads/2009 - 2022 PIT Count Totals.xlsx", skip = 2)

## Prep Variables
pit <- pit %>% 
  clean_names() %>% 
  na.omit(homeless_count) # remove blank row

# create total sheltered
pit <- pit %>% 
  mutate(year = year(date_of_count),
         sheltered_total_persons = sheltered_es_total_persons + sheltered_th_total_persons,
         ) %>% 
  select(-c(status_of_report, sheltered_sh_total_persons))

# pivot to make sheltered/unsheltered one column
pit_long <- pit %>% 
  select(-c(sheltered_es_total_persons, sheltered_th_total_persons)) %>% 
  pivot_longer(cols = c(unsheltered_total_persons, sheltered_total_persons, total_persons),
               names_to = "type", values_to = "count")

# ..................................................
# Review Background & Issues ---- 
glimpse(pit)

# have a peek
ggplot(pit_long, aes(x = year, y = count, color = type)) + geom_line()

# ..................................................

# Export Data ----
write_csv(pit_long, "data/pitcount_homeless.csv")
# pit_long <- read_csv("data/pitcount_homeless.csv")
