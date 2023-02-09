# Stepping Stones Data: Infant Mortality ----
# Updated 2023-01-16
# Contributor: Michele Claibourn
# Acquire data from VDH Health Stats site: Infant mortality
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Health, Division of Health Statistics, "Total Infant Deaths by Place of Occurrence and Place of Residence." 2000-2022.
# https://apps.vdh.virginia.gov/HealthStats/stats.htm

# Libraries ----
library(tidyverse)
library(tabulizer)
library(zoo)


# Read in a single pdf report ----
# ## Infant mortality ----
# pdf20 <- "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/inf_1-1_2020.pdf"
# # define area to read
# pdf20area <- locate_areas(pdf20, 1) # selected manually
# list20 <- extract_tables(pdf20, area = pdf20area, pages = 1,
#                          guess = FALSE, output = "data.frame")
# 
# df20 <- list20[[1]]
# names(df20) <- c("locality", "numdeaths1", "numdeaths1_white", 
#                  "numdeaths1_black", "numdeaths1_other",
#                  "numdeaths", "numdeaths_white", "numdeaths_black",
#                  "numdeaths_other", "ratedeaths", "ratedeaths_white",
#                  "ratedeaths_black", "ratedeaths_other")
# 
# loc20 <- df20 %>% 
#   filter(locality %in% c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY", "COMMONWEALTH OF VIRGINIA")) %>% 
#   mutate(year = 2020, locality = str_to_title(locality)) %>% 
#   select(locality, numdeaths, ratedeaths, year)
# 
# # this chooses total in place of residence (also contains total in place of occurrence)

# Make a function ----
# need to define pdf area 

# extract_appvdh: pull state, locality values from app vdh pdf
# arguments: pdfurl, year, localities
extract_appvdh <- function(pdfurl, year, localities) {
  
  list <- extract_tables(pdfurl, area = pdfarea, pages = 1,
                           guess = FALSE, output = "data.frame")
  dflist <- list[[1]]
  names(dflist) <- c("locality", "numdeaths", "numdeaths_white", 
                   "numdeaths_black", "numdeaths_other",
                   "numdeaths2", "numdeaths2_white", "numdeaths2_black",
                   "numdeaths2_other", "ratedeaths", "ratedeaths_white",
                   "ratedeaths_black", "ratedeaths_other")
  
  df <- dflist %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, numdeaths, ratedeaths, year)
  
  return(df)
}


# Generate a list of urls ----
# 2000-2013 follow a consistent pattern, 2018-2020 follow a consistent new pattern
yearsa <- tibble(y = seq(0,13,1)) %>% 
  mutate(y = as.character(y),
         y = str_pad(y, width = 2, side = "left", pad = "0"))
pdflista <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/InfDeathRace",yearsa$y,".pdf")

yearsb <- tibble(y = seq(2018,2020,1)) %>% 
  mutate(y = as.character(y))
pdflistb <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/pdf/inf_1-1_",yearsb$y,".pdf")

pdflist <- c(pdflista, 
             "https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/inf_1-1.pdf", 
             "https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/bk1inf01.pdf",
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2016/2016-inf_1-1.pdf",
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2017/inf_1-1.pdf",
             pdflistb)

# 2004 and earlier pdfs look different; not sure if it will work
# 2016-2014 doesn't work; 2005-2000 doesn't work...
# the table area in the pdfs are higher/wider... do these individually (with new pdfarea defined)


# Apply the function ----
# pdfarea <- locate_areas(pdflist[21], 1) # selected manually
pdfarea <- list(setNames(c(76, 35, 760, 590), 
                         c("top", "left", "bottom", "right")))
# make a data frame of paired url and year arguments
pdf_year <- tibble(pdf = pdflist, year = seq(2000,2020,1))

# remove years that don't work
pdf_year <- pdf_year[c(7:14,18:21),]

# apply function across the url/year pairs, add countyfips argument
infmort_list <- map2(pdf_year$pdf, pdf_year$year, 
                  ~extract_appvdh(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
infmort <- bind_rows(infmort_list)


# Read in missing years ----
## 2014-2016 ---- 
# define area to read
# pdfarea <- locate_areas(pdflist[15], 1) # selected manually
pdfarea <- list(setNames(c(80, 34, 720, 590), 
                         c("top", "left", "bottom", "right")))

pdf_yearb <- tibble(pdf = pdflist, year = seq(2000,2020,1))
# 2014-2016 only
pdf_yearb <- pdf_yearb[c(15:17),]

# apply function across the url/year pairs, add countyfips argument
infmort_listb <- map2(pdf_yearb$pdf, pdf_yearb$year, 
                     ~extract_appvdh(.x, .y, localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

# bind the list of resulting data frames into a single dataframe
infmortb <- bind_rows(infmort_listb)


## 2000-2005 ---- 
### 2005 ----
# define area to read
# pdfarea <- locate_areas(pdflist[6], 1) # selected manually
pdfarea <- list(setNames(c(83, 38, 780, 590), 
                         c("top", "left", "bottom", "right")))

df05 <- extract_appvdh(pdflist[6], 2005, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


### 2004 ----
# define area to read
# pdfarea <- locate_areas(pdflist[5], 1) # selected manually
pdfarea <- list(setNames(c(78, 38, 725, 590), 
                         c("top", "left", "bottom", "right")))

df04 <- extract_appvdh(pdflist[5], 2004, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


### 2003 ----
# define area to read
# pdfarea <- locate_areas(pdflist[4], 1) # selected manually
pdfarea <- list(setNames(c(77, 40, 745, 590), 
                         c("top", "left", "bottom", "right")))

df03 <- extract_appvdh(pdflist[4], 2003, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


### 2002 ----
# pdfarea <- locate_areas(pdflist[3], 1) # selected manually
pdfarea <- list(setNames(c(90, 18, 720, 600), 
                         c("top", "left", "bottom", "right")))

df02 <- extract_appvdh(pdflist[3], 2002, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


### 2001 ----
# pdfarea <- locate_areas(pdflist[2], 1) # selected manually
pdfarea <- list(setNames(c(97, 18, 730, 600), 
                         c("top", "left", "bottom", "right")))

df01 <- extract_appvdh(pdflist[2], 2001, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


### 2000 ----
# pdfarea <- locate_areas(pdflist[1], 1) # selected manually
pdfarea <- list(setNames(c(92, 99, 661, 520), 
                         c("top", "left", "bottom", "right")))

df00 <- extract_appvdh(pdflist[1], 2000, c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))


## combine all data frames ----
inf_mort_allyears <- bind_rows(infmort, infmortb,
                              df05, df04, df03, df02, df01, df00) %>% 
  replace_na(list(numdeaths = 0, ratedeaths = 0))


# And have a peek
ggplot(inf_mort_allyears, aes(x = year, y = ratedeaths, color = locality)) +
  geom_line()


## Create three-year rolling average ----
inf_mort_allyears <- inf_mort_allyears %>% 
  arrange(locality, year) %>% 
  group_by(locality) %>% 
  mutate(rate_3yr = zoo::rollmean(ratedeaths, k = 3, fill = NA, align = "center")) %>% 
  ungroup()


# Save data ----
write_csv(inf_mort_allyears, "data/infant_mortality.csv")
