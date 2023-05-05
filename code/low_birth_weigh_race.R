# Stepping Stones Data: Low Birth Weight ----
# Updated 2023-05-05
# Contributor: Michele Claibourn
# Acquire data from VDH Health Stats site: Low birth weight births by rce
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Health, Division of Health Statistics, "Resident Low Weight Live Births and Very Low Weight Births." 2000-2022.
# https://apps.vdh.virginia.gov/HealthStats/stats.htm

# Libraries ----
library(tidyverse)
library(tabulizer)
library(zoo)


# # Read in a single pdf report ----
# # ## low birth weight ----
# pdf20 <- "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/birth_1-10_2020.pdf"
# # define area to read
# # pdf20area <- locate_areas(pdf20, 1) # selected manually
# pdfarea <- list(setNames(c(68, 25, 680, 590),
#                          c("top", "left", "bottom", "right")))
# 
# list20 <- extract_tables(pdf20, area = pdf20area, pages = 1,
#                          guess = FALSE, output = "data.frame")
# 
# df20 <- list20[[1]]
# names(df20) <- c("locality", "num_lbw", "num_lbw_white", "num_lbw_black", "num_lbw_other",
#                  "pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other",
#                  "num_vlbw", "num_vlbw_white", "num_vlbw_black", "num_vlbw_other",
#                  "pct_vlbw", "pct_vlbw_white", "pct_vlbw_black", "pct_vlbw_other")
# 
# loc20 <- df20 %>%
#   filter(locality %in% c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY", "COMMONWEALTH OF VIRGINIA")) %>%
#   mutate(year = 2020, locality = str_to_title(locality)) %>%
#   select(locality, num_lbw, num_lbw_white, num_lbw_black, num_lbw_other, 
#          pct_lbw, pct_lbw_white, pct_lbw_black, pct_lbw_other, year)


# Generate a list of urls ----
# 2020: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/birth_1-10_2020.pdf
# 2019, 2018: same
# 2017: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2017/birth_1-10.pdf
# 2016: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2016/2016-birth_1-10.pdf
# 2015: https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/birth_1-10.pdf
# 2014: https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/LWBirths14.pdf
# 2013-2000: same

# 2000-2014
yearsa <- tibble(y = seq(0,14,1)) %>% 
  mutate(y = as.character(y),
         y = str_pad(y, width = 2, side = "left", pad = "0"))
pdflista <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/LWBirths",yearsa$y,".pdf")

# 2018-2020
yearsb <- tibble(y = seq(2018,2020,1)) %>% 
  mutate(y = as.character(y))
pdflistb <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/pdf/birth_1-10_",yearsb$y,".pdf")

pdflist <- c(pdflista, 
             "https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/birth_1-10.pdf", 
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2016/2016-birth_1-10.pdf",
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2017/birth_1-10.pdf",
             pdflistb)

# Make a function ----
# need to define pdf area 
# pdfarea <- locate_areas(pdf20, 1) # selected manually
# defined from above
pdfarea <- list(setNames(c(68, 25, 680, 590),
                         c("top", "left", "bottom", "right")))

# extract_appvdh: pull state, locality values from app vdh pdf
# arguments: pdfurl, year, localities
extract_appvdh <- function(pdfurl, year, localities) {
  
  list <- extract_tables(pdfurl, area = pdfarea, pages = 1,
                         guess = FALSE, output = "data.frame")
  dflist <- list[[1]]
  names(dflist) <- c("locality", "num_lbw", "num_lbw_white", "num_lbw_black", "num_lbw_other",
                   "pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", 
                   "num_vlbw", "num_vlbw_white", "num_vlbw_black", "num_vlbw_other",
                   "pct_vlbw", "pct_vlbw_white", "pct_vlbw_black", "pct_vlbw_other")
  
  df <- dflist %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, num_lbw, num_lbw_white, num_lbw_black, num_lbw_other,
           pct_lbw, pct_lbw_white, pct_lbw_black, pct_lbw_other, year)
  return(df)
}


# Apply the function ----
## 2017-2021 ----
# make a data frame of paired url and year arguments
pdf_year <- tibble(pdf = pdflist, year = seq(2000,2020,1))

# remove years that don't work
pdf_year_a <- pdf_year[c(18:21),]

# apply function across the url/year pairs, add countyfips argument
lbw_list <- map2(pdf_year_a$pdf, pdf_year_a$year, 
                     ~extract_appvdh(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
lbw <- bind_rows(lbw_list)

## Adapt pdfarea for 2014-2016 ----
# pdfarea_b <- locate_areas(pdflist[17], 1) # selected manually
# defined from above
pdfarea_b <- list(setNames(c(80, 25, 690, 590),
                         c("top", "left", "bottom", "right")))

# arguments: pdfurl, year, localities
extract_appvdh_b <- function(pdfurl, year, localities) {
  
  list <- extract_tables(pdfurl, area = pdfarea_b, pages = 1,
                         guess = FALSE, output = "data.frame")
  dflist <- list[[1]]
  names(dflist) <- c("locality", "num_lbw", "num_lbw_white", "num_lbw_black", "num_lbw_other",
                     "pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", 
                     "num_vlbw", "num_vlbw_white", "num_vlbw_black", "num_vlbw_other",
                     "pct_vlbw", "pct_vlbw_white", "pct_vlbw_black", "pct_vlbw_other")
  
  df <- dflist %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, num_lbw, num_lbw_white, num_lbw_black, num_lbw_other,
           pct_lbw, pct_lbw_white, pct_lbw_black, pct_lbw_other, year)
  
  return(df)
}

## 2014-2016
# worked for 2022, 2016-2014
pdf_year_b <- pdf_year[c(15:17),]
# apply function across the url/year pairs, add countyfips argument
lbw_list_b <- map2(pdf_year_b$pdf, pdf_year_b$year, 
                 ~extract_appvdh_b(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
lbw_b <- bind_rows(lbw_list_b)


## Adapt pdfarea for 2005-2013 ----
# pdfarea_c <- locate_areas(pdflist[14], 1) # selected manually
# defined from above
pdfarea_c <- list(setNames(c(77, 18, 770, 600),
                           c("top", "left", "bottom", "right")))

# arguments: pdfurl, year, localities
extract_appvdh_c <- function(pdfurl, year, localities) {
  
  list <- extract_tables(pdfurl, area = pdfarea_c, pages = 1,
                         guess = FALSE, output = "data.frame")
  dflist <- list[[1]]
  names(dflist) <- c("locality", "num_lbw", "num_lbw_white", "num_lbw_black", "num_lbw_other",
                     "pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", 
                     "num_vlbw", "num_vlbw_white", "num_vlbw_black", "num_vlbw_other",
                     "pct_vlbw", "pct_vlbw_white", "pct_vlbw_black", "pct_vlbw_other")
  
  df <- dflist %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, num_lbw, num_lbw_white, num_lbw_black, num_lbw_other,
           pct_lbw, pct_lbw_white, pct_lbw_black, pct_lbw_other, year)
  
  return(df)
}

## 2005-2013
pdf_year_c <- pdf_year[c(6:14),]
# apply function across the url/year pairs, add countyfips argument
lbw_list_c <- map2(pdf_year_c$pdf, pdf_year_c$year, 
                   ~extract_appvdh_c(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
lbw_list_c <- map(lbw_list_c, ~mutate_all(., as.character))

lbw_c <- bind_rows(lbw_list_c)


## Adapt pdfarea for 2000-2004 ----
# pdfarea_d <- locate_areas(pdflist[1], 1) # selected manually
# defined from above
pdfarea_d <- list(setNames(c(80, 20, 700, 370),
                           c("top", "left", "bottom", "right")))

# arguments: pdfurl, year, localities
extract_appvdh_d <- function(pdfurl, year, localities) {
  
  list <- extract_tables(pdfurl, area = pdfarea_c, pages = 1,
                         guess = FALSE, output = "data.frame")
  dflist <- list[[1]]
  names(dflist) <- c("locality", "num_lbw", "num_lbw_white", "num_lbw_black", "num_lbw_other",
                     "pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", 
                     "num_vlbw", "num_vlbw_white", "num_vlbw_black", "num_vlbw_other",
                     "pct_vlbw", "pct_vlbw_white", "pct_vlbw_black", "pct_vlbw_other")
  
  df <- dflist %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, num_lbw, num_lbw_white, num_lbw_black, num_lbw_other,
           pct_lbw, pct_lbw_white, pct_lbw_black, pct_lbw_other, year)
  
  return(df)
}

## 2000-2004
pdf_year_d <- pdf_year[c(1:5),]
# apply function across the url/year pairs, add countyfips argument
lbw_list_d <- map2(pdf_year_d$pdf, pdf_year_d$year, 
                   ~extract_appvdh_d(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
lbw_d <- bind_rows(lbw_list_d)

# fix collapsed column (pct_lbw_white and pct_lbw_black)
lbw_d <- lbw_d %>% 
  separate(pct_lbw_white, into = c("pct_lbw_white", "pct_lbw_black"), sep = " ")


# Bind the years together ----
# format for binding
lbw_c <- lbw_c %>% 
  mutate(across(c("pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", "year"), as.numeric))

lbw_d <- lbw_d %>% 
  mutate(across(c("pct_lbw", "pct_lbw_white", "pct_lbw_black", "pct_lbw_other", "year"), as.numeric),
         num_lbw_other = as.character(num_lbw_other))

lbw_all <- bind_rows(lbw, lbw_b, lbw_c, lbw_d)

# remove commas from numbers; fill in missing and blank with 0
lbw_all <- lbw_all %>% 
  mutate(across(contains("num_lbw"), ~str_remove(.x, ",")),
         across(contains("other"), ~ifelse(is.na(.x)|.x == "", 0, .x)))
  
# And have a peek
ggplot(lbw_all, aes(x = year, y = pct_lbw, color = locality)) +
  geom_line()


## Create three-year rolling average ----
lbw_all <- lbw_all %>% 
  arrange(locality, year) %>% 
  group_by(locality) %>% 
  mutate(pct_all_3yr = zoo::rollmean(pct_lbw, k = 3, fill = NA, align = "center"),
         pct_white_3yr = zoo::rollmean(pct_lbw_white, k = 3, fill = NA, align = "center"),
         pct_black_3yr = zoo::rollmean(pct_lbw_black, k = 3, fill = NA, align = "center"),
         pct_other_3yr = zoo::rollmean(pct_lbw_other, k = 3, fill = NA, align = "center")) %>% 
  ungroup()

# And have a peek
ggplot(lbw_all, aes(x = year, y = pct_all_3yr, color = locality)) +
  geom_line()

# for race, will need to make long...
lbw_all %>% 
  select(locality, year, pct_all_3yr:pct_other_3yr) %>% 
  pivot_longer(-c(locality, year), names_to = "group", names_prefix = "pct_", values_to = "percent") %>% 
  ggplot(aes(x = year, y = percent, color = group)) +
  geom_line() +
  facet_wrap(~ locality)

# Save data ----
write_csv(lbw_all, "data/low_birth_weight_race.csv")
