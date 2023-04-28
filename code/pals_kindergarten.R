# Title: Stepping Stones Project
# Education-Kindergarten PALS
# Team 5: Owayne Owens, Amruta Binoy, Mary Katherine West; Michele Claibourn
# Last Updated: 2023-04-26 - Beth made changes

# Proposed Citation
# Anne E. Casey Foundation, Kids Count Data Center,  "Fall PALS-K before and after 2015/16 in Virginia", 
# https://datacenter.kidscount.org/data/tables/3254-fall-pals-k-before-2015-16
# https://datacenter.kidscount.org/data/tables/10181-fall-pals-k-after-2015-16
# Accessed 2023-02-15

# ..................................................
# Load Libraries ----
library(tidyverse)
library(janitor)
library(readxl)
library(stringi)

# ..................................................
# Download/Read in Data ----
# downloading data via the url link
url2 <- 'https://datacenter.kidscount.org/rawdata.axd?ind=10181&loc=48'
url3 <- 'https://datacenter.kidscount.org/rawdata.axd?ind=3254&loc=48'
download.file(url2, "datadownloads/kindergarten_children_for_reading_intervention_2016-2020.xlsx")
download.file(url3, "datadownloads/kindergarten_children_for_reading_intervention_2002-2015.xlsx")

# reading in the data 
k_reading_af15 <- read_excel("datadownloads/kindergarten_children_for_reading_intervention_2016-2020.xlsx")
k_reading <- read_excel("datadownloads/kindergarten_children_for_reading_intervention_2002-2015.xlsx")

# joining the two dataframes into one by using the rbind command
k_reading <- rbind(k_reading, k_reading_af15)

# removing the dataframe with just values after 2015 since it has been joined to the dataframe with values before 2015
# rm(k_reading_af15)

# ..................................................
# Clean Variables ----
# observing the data
view (k_reading)
glimpse (k_reading)

# clean the data names
k_reading <- clean_names(k_reading)

#renaming the "time_frame" variable to "year" - going to just make a new variable for school year, keep this as is
# k_reading <- k_reading %>% 
#   rename (year = time_frame)

#filtering the dataframe to only store data points from Charlottesville, Albemarle, and Virginia
k_reading <- k_reading %>% 
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville"))

# Pivot to put numbers and percents in separate columns
k_reading_wide <- k_reading %>%
  pivot_wider(names_from = data_format, values_from = data)

# making new column names (percent and number) lowercase
k_reading_wide <- k_reading_wide %>% 
  clean_names()

# changing percent and number from character to numeric; making percents scale to 100
k_reading_wide <- k_reading_wide %>% 
  mutate(percent = as.numeric(percent)*100,
         number = as.numeric(number))

# recoding the values stored under the year variable to a common/simpler format. 
# Beth note: this original process seems to have resulted in NAs for some years that had data, 
# plus, I just want the school year as characters anyway
k_reading_wide <- k_reading_wide %>%
  mutate(school_year = str_remove(time_frame, "AY |FY "), # get rid of the AY/FY
         school_year = stri_sub_replace(school_year, 6, 5, value = "20"), # add "20" in front of the second year in the span
         school_year = substr(school_year, 1, 9) # keep only the characters needed/rm the extra "20" that ends up in 2019-2020
         # previous version below
         # school_year = year, # keep the original variable a new column
         # year = str_remove(year, "AY |FY "), # get rid of the AY/FY
         # year = str_sub(year, 1,4), # to grab first year, since it's the only one with the 20 part
         # year = as.numeric(year),
         # year = year+1 # to make it into the end of the school year
         ) 

# ..................................................
# Save data ---- (beth note: created a new file to keep the original)
write_csv(k_reading_wide, "data/pals_kindergarten_updated.csv")
# write_csv(k_reading_wide, "data/pals_kindergarten.csv")

