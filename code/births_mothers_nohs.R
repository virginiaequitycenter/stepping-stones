# Title: Stepping Stones Project
# Infant health-Births to mothers with less than a 12th grade education (% of live births) 
# Team 6: Connor Eads, Maria Morrissey, Vani Agarwal, Melinda Wong
# Team 5: Owayne Owens, Amruta Binoy, Mary Katherine West
# Last Updated: 2023-02-15

# Proposed Citation
# Anne E. Casey Foundation, Kids Count Data Center, "Births to mothers with less than 12 years of education." 2000-2020
# https://datacenter.kidscount.org/data/tables/3257-births-to-mothers-with-less-than-12th-grade-education
# Accessed 2023-02-15

# ..................................................
# Load Libraries, Read in Data, & Clean Variables ----

library(tidyverse)
library(janitor)
library(readxl)
 
## CSV (URL to Excel)
url <- "https://datacenter.kidscount.org/rawdata.axd?ind=3257&loc=48"
dest <- "datadownloads/births_mothers_nohs.xlsx"
download.file(url, dest)

nohs <- read_excel("datadownloads/births_mothers_nohs.xlsx", sheet = 1)

## Clean Variable: 

nohs <- clean_names(nohs)

## Filter for Albemarle, Charlottesville, and Virginia

nohs_cvlalb <- nohs %>% 
  filter(location %in% c("Virginia", "Charlottesville", "Albemarle"))

# ..................................................
# Review Background & Issues ---- 

glimpse(nohs_cvlalb)

## Issues: structure of "data" and "time_frame" columns 

nohs_cvlalb <- nohs_cvlalb %>% 
  mutate(percent = as.numeric(data) *100,
         year = as.numeric(time_frame)) %>% 
  select(year, location, percent)

# have a peek
ggplot(nohs_cvlalb, aes(x = year, y = percent, color = location)) + geom_line()

# ..................................................

# Export Data ----
write_csv(nohs_cvlalb, "data/births_mothers_nohs.csv")
