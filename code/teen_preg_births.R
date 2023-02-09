# Stepping Stones Data: Low Birth Weight ----
# Updated 2023-01-22
# Contributor: Michele Claibourn
# Acquire data from VDH Health Stats site: Teen pregnancy, births to teens
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Health, Division of Health Statistics, "Teenage pregnancies, live births by city/county.." 2000-2022.
# https://apps.vdh.virginia.gov/HealthStats/stats.htm

# Notes: could try using pdftools::pdf_data to find and calculate pdf area
#    like this example -- https://redwallanalytics.com/2020/04/06/tabulizer-and-pdftools-together-as-super-powers-part-2/


# Libraries ----
library(tidyverse)
library(tabulizer)
library(zoo)


# # Read in a single pdf report ----
# pdf20 <- "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/preg_3-8_2020.pdf"
# # define area to read
# # pdfarea <- locate_areas(pdf20, 1) # selected manually
# # pdfarea <- locate_areas(pdf20, 2) # selected manually
# # defined from above
# pdfarea1 <- list(setNames(c(70, 20, 760, 600),
#                           c("top", "left", "bottom", "right")))
# pdfarea2 <- list(setNames(c(70, 20, 760, 600),
#                           c("top", "left", "bottom", "right")))
# 
# # read table
# list19a <- extract_tables(pdflist[20], pages = 1, area = pdfarea1,
#                          guess = FALSE, output = "data.frame")
# list19b <- extract_tables(pdflist[20], pages = 2, area = pdfarea2,
#                           guess = FALSE, output = "data.frame")
#  
# dfnum19 <- list19a[[1]][,1:9]
# dfrate19 <- list19b[[1]][,c(1:8,17)]
# names(dfnum19) <- c("locality", "pregnancies", "under15pregnancies", "from15to17pregnancies", "from18to19pregnancies",
#                     "births", "under15births", "from15to17births", "from18to19births")
# names(dfrate19) <- c("pregnancyrate", "under15pregnancyrate", "from15to17pregnancyrate", "from18to19pregnancyrate",
#                     "birthsrate", "under15birthsrate", "from15to17birthsrate", "from18to19birthsrate",
#                     "locality")
#  
# loc19a <- dfnum19 %>%
#   filter(locality %in% c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY", "COMMONWEALTH OF VIRGINIA")) %>%
#   mutate(year = 2019, locality = str_to_title(locality)) %>%
#   select(locality, pregnancies, from15to17births, year)
# 
# loc19b <- dfrate19 %>%
#   filter(locality %in% c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY", "COMMONWEALTH OF VIRGINIA")) %>%
#   mutate(year = 2019, locality = str_to_title(locality)) %>%
#   select(locality, pregnancyrate, from15to17birthsrate, year)
# 
# loc19 <- left_join(loc19a, loc19b)

# Generate a list of urls ----
# 2020-2018: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/preg_3-8_2020.pdf
# 2017: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2017/preg_3-8.pdf
# 2016: https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2016/2016-preg_3_8.pdf
# 2015: https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/preg_3_8_web.pdf
# 2014: https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/PregSup%20Tbl%2008%20(web%20only).pdf
# 2013-2000: https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/TeenPregAge13.pdf

# 2000-2013
yearsa <- tibble(y = seq(0,13,1)) %>% 
  mutate(y = as.character(y),
         y = str_pad(y, width = 2, side = "left", pad = "0"))
pdflista <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/TeenPregAge",yearsa$y,".pdf")

# 2018-2020
yearsb <- tibble(y = seq(2018,2020,1)) %>% 
  mutate(y = as.character(y))
pdflistb <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/pdf/preg_3-8_",yearsb$y,".pdf")

pdflist <- c(pdflista, 
             "https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/PregSup%20Tbl%2008%20(web%20only).pdf", 
             "https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/preg_3_8_web.pdf",
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2016/2016-preg_3_8.pdf",
             "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/2017/preg_3-8.pdf",
             pdflistb)

# Make a function ----
# extract_appvdh: pull state, locality values from app vdh pdf
# arguments: pdfurl, year, localities
extract_appvdh <- function(pdfurl, year, localities, pdfarea1, pdfarea2) {
  
  lista <- extract_tables(pdfurl, pages = 1, area = pdfarea1,
                         guess = FALSE, output = "data.frame")
  listb <- extract_tables(pdfurl, pages = 2, area = pdfarea2,
                          guess = FALSE, output = "data.frame")

  dflista <- lista[[1]][,1:9]
  dflistb <- listb[[1]][,c(1:8,17)]
  names(dflista) <- c("locality", "pregnancies", "under15pregnancies", "from15to17pregnancies", "from18to19pregnancies",
                      "births", "under15births", "from15to17births", "from18to19births")
  names(dflistb) <- c("pregnancyrate", "under15pregnancyrate", "from15to17pregnancyrate", "from18to19pregnancyrate", 
                      "birthsrate", "under15birthsrate", "from15to17birthsrate", "from18to19birthsrate",
                      "locality")

  dfa <- dflista %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, pregnancies, from15to17births, year)
  
  dfb <- dflistb %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, locality = str_to_title(locality)) %>% 
    select(locality, pregnancyrate, from15to17birthsrate, year)
  
  df <- left_join(dfa, dfb)
  
  return(df)
}

# Apply function ----
## 2020 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[21], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[21], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(70, 20, 760, 600), 
                         c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(70, 20, 760, 600), 
                          c("top", "left", "bottom", "right")))

vdh20 <- extract_appvdh(pdflist[21], 2020, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2019 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[20], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[20], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(67, 24, 700, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(70, 24, 700, 590), 
                          c("top", "left", "bottom", "right")))

vdh19 <- extract_appvdh(pdflist[20], 2019, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2018 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[19], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[19], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(77, 23, 760, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(76, 25, 760, 590), 
                          c("top", "left", "bottom", "right")))

vdh18 <- extract_appvdh(pdflist[19], 2018, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2017 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[18], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[18], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(69, 22, 700, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(68, 24, 700, 590), 
                          c("top", "left", "bottom", "right")))

vdh17 <- extract_appvdh(pdflist[18], 2017, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2016 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[17], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[17], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(80, 24, 715, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(80, 25, 715, 590), 
                          c("top", "left", "bottom", "right")))

vdh16 <- extract_appvdh(pdflist[17], 2016, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2015 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[16], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[16], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(68, 22, 700, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(68, 25, 700, 590), 
                          c("top", "left", "bottom", "right")))

vdh15 <- extract_appvdh(pdflist[16], 2015, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2014 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[15], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[15], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(68, 23, 700, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(68, 25, 700, 590), 
                          c("top", "left", "bottom", "right")))

vdh14 <- extract_appvdh(pdflist[15], 2014, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2013 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[14], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[14], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(74, 23, 765, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(75, 22, 765, 590), 
                          c("top", "left", "bottom", "right")))

vdh13 <- extract_appvdh(pdflist[14], 2013, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2012 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[13], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[13], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(76, 22, 765, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(75, 23, 765, 590), 
                          c("top", "left", "bottom", "right")))

vdh12 <- extract_appvdh(pdflist[13], 2012, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2011 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[12], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[12], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 23, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(73, 21, 768, 590), 
                          c("top", "left", "bottom", "right")))

vdh11 <- extract_appvdh(pdflist[12], 2011, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2010 ----
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[11], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[11], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 20, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(72, 23, 768, 590), 
                          c("top", "left", "bottom", "right")))

vdh10 <- extract_appvdh(pdflist[11], 2010, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2009 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[10], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[10], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 22, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(73, 22, 770, 595), 
                          c("top", "left", "bottom", "right")))

vdh09 <- extract_appvdh(pdflist[10], 2009, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2008 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[9], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[9], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 24, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(74, 24, 770, 590), 
                          c("top", "left", "bottom", "right")))

vdh08 <- extract_appvdh(pdflist[9], 2008, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2007 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[8], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[8], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 23, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(73, 24, 770, 590), 
                          c("top", "left", "bottom", "right")))

vdh07 <- extract_appvdh(pdflist[8], 2007, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2006 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[7], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[7], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 20, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(74, 22, 770, 590), 
                          c("top", "left", "bottom", "right")))

vdh06 <- extract_appvdh(pdflist[7], 2006, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2005 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[6], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[6], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(75, 22, 770, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(74, 22, 770, 590), 
                          c("top", "left", "bottom", "right")))

vdh05 <- extract_appvdh(pdflist[6], 2005, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2004 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[5], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[5], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(72, 24, 720, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(72, 24, 725, 590), 
                          c("top", "left", "bottom", "right")))

vdh04 <- extract_appvdh(pdflist[5], 2004, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2003 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[4], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[4], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(72, 23, 740, 590), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(71, 25, 742, 590), 
                          c("top", "left", "bottom", "right")))

vdh03 <- extract_appvdh(pdflist[4], 2003, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2002 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[3], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[3], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(85, 16, 720, 600), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(83, 22, 720, 600), 
                          c("top", "left", "bottom", "right")))

vdh02 <- extract_appvdh(pdflist[3], 2002, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2001 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[2], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[2], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(98, 17, 740, 600), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(98, 17, 740, 600), 
                          c("top", "left", "bottom", "right")))

vdh01 <- extract_appvdh(pdflist[2], 2001, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)

## 2000 ---- 
# define pdfarea for page 1 and 2, each year; input in function
# pdfarea1 <- locate_areas(pdflist[1], 1) # selected manually
# pdfarea2 <- locate_areas(pdflist[1], 2) # selected manually
# defined from above
pdfarea1 <- list(setNames(c(82, 75, 657, 540), 
                          c("top", "left", "bottom", "right")))
pdfarea2 <- list(setNames(c(80, 75, 655, 545), 
                          c("top", "left", "bottom", "right")))

vdh00 <- extract_appvdh(pdflist[1], 2000, 
                        c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"), 
                        pdfarea1, pdfarea2)


# Bind the years together ----
# put data frames into list
df_list = mget(ls(pattern = "^vdh"))
# make all columns character (inconsistencies in var type prevents row bind)
df_list <- map(df_list, ~mutate_all(., as.character))
# bind all data frames
vdh_all <- bind_rows(df_list)
# format variables
vdh_all <- vdh_all %>% 
  mutate(pregnancies = as.numeric(gsub(",", "", pregnancies)),
         from15to17births = as.numeric(gsub(",", "", from15to17births)),
         year = as.numeric(year),
         pregnancyrate = as.numeric(pregnancyrate),
         from15to17birthsrate = as.numeric(from15to17birthsrate))

## And have a peek
ggplot(vdh_all, aes(x = year, y = pregnancyrate, color = locality)) +
  geom_line()

ggplot(vdh_all, aes(x = year, y = from15to17birthsrate, color = locality)) +
  geom_line()

# Create three year rolling average ----
vdh_all <- vdh_all %>% 
  arrange(locality, year) %>% 
  group_by(locality) %>% 
  mutate(pregnancyrate_3yr = zoo::rollmean(pregnancyrate, k = 3, fill = NA, align = "center"),
         birthsrate_3yr = zoo::rollmean(from15to17birthsrate, k = 3, fill = NA, align = "center")) %>% 
  ungroup()

## And have a peek
ggplot(vdh_all, aes(x = year, y = pregnancyrate_3yr, color = locality)) +
  geom_line()

ggplot(vdh_all, aes(x = year, y = birthsrate_3yr, color = locality)) +
  geom_line()


# Save data ----
write_csv(vdh_all, "data/teen_pregnancy_birth_rates.csv")

