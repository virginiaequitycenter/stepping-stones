# Stepping Stones Data: Voter Registration ----
# Updated 2023-01-13
# Contributors: Michele Claibourn
# Acquire voter registration numbers
# Charlottesville, Albemarle, State

# Data captured in a series of inconsistent yearly formats here
# https://www.elections.virginia.gov/resultsreports/registration-statistics/
#   From 2000-2006, data is in pdf table
#   In 2007, data is in pdf pages (one page per locality)
#   In 2008, data is in excel table
#   From 2009-2012, data excel tabs (one tab per locality)
#   From 2013-2021, data is in csv table
#
# Proposed Citation: 
# Virginia Department of Elections, "Registration Statistics, 2000-2022."  
# https://www.elections.virginia.gov/resultsreports/registration-statistics/
#
# Worked out how to read each in voter_reg_initial.R;
#.  making it more systematic here


# Libraries ----
library(tidyverse)
library(tabulizer)
library(readxl)


# 2013-2021 csv tables ----
# the easiest!

# ## Test ----
# ss <- "https://www.elections.virginia.gov/media/registration-statistics/2021/01/csv20210201/Daily_Registrant_Count_By_Locality_2021_02_01_054110.csv"
# vr <- read_csv(ss) # read in spreadsheet
# 
# test <- vr %>% 
#   mutate(Locality = str_replace(Locality, "Locality: ", ""), # remove leading label in Locality
#          fips = str_sub(Locality, 1,3), # extract fips code into another variable
#          Locality = str_remove_all(Locality, "[0-9]"), # remove fips code from Locality
#          Locality = str_trim(Locality), # remove leading white space from locality name
#          Locality = str_to_title(Locality)) %>% # change locality name to title case
#   filter(str_detect(Locality, "Albemarle|Charlottesville")) %>% # filter for localities by name
#   group_by(Locality, fips) %>% # group by remaining localities
#   summarize(voters = sum(AllVoters)) %>% # sum the registered voters across precincts
#   mutate(year = 2021) %>% # add year of data
#   select(locality=Locality, fips, voters, year) %>% # keep variables
#   bind_rows(vr %>% 
#               summarize(voters = sum(AllVoters)) %>% # create state total
#               mutate(year = 2021, fips = "51", locality = "Virginia") %>% 
#               select(locality, fips, voters, year)) # and bind to locality totals

## Make a function ----
# extract_regvoters: output selected locality registration totals
# arguments: url, year, localities

extract_regvoters <- function(url, year, localities) {
  votereg <- read_csv(url)
  loc <- str_flatten(localities, collapse = "|")
  
  df <- votereg %>% 
    mutate(Locality = str_replace(Locality, "Locality: ", ""),
           fips = str_sub(Locality, 1,3),
           Locality = str_remove_all(Locality, "[0-9]"),
           Locality = str_trim(Locality),
           Locality = str_to_title(Locality)) %>% 
    filter(str_detect(Locality, loc)) %>% 
    group_by(Locality, fips) %>% 
    summarize(voters = sum(AllVoters)) %>% 
    mutate(year = year) %>% 
    select(locality=Locality, fips, voters, year) %>% 
    bind_rows(votereg %>% 
                summarize(voters = sum(AllVoters)) %>% 
                mutate(year = year, fips = "51", locality = "Virginia") %>% 
                select(locality, fips, voters, year))
  
  return(df)
}

### Apply to 2013-2021 ----
### Create list of urls
yearsa <- tibble(y = seq(2013,2019,1)) %>% 
  mutate(y = as.character(y))
urllista <- paste0("https://www.elections.virginia.gov/media/registration-statistics/",yearsa$y,"/12/Registrant_Count_By_Locality.csv")

urllist <- c(urllista,
             "https://www.elections.virginia.gov/media/registration-statistics/2020/dec/Daily_Registrant_Count_By_Locality_2021_01_01_053053.csv", # 2020
             "https://www.elections.virginia.gov/media/registration-statistics/2021/12/dec2021-csv-stats/Daily_Registrant_Count_By_Locality_2022_01_01_053216.csv") # 2021
  
url_year <- tibble(url = urllist, year = seq(2013,2021,1))

### apply function to list
vr_list <- map2(url_year$url, url_year$year, 
                     ~extract_regvoters(.x, .y, localities = c("Albemarle", "Charlottesville"))) 

vr1323 <- bind_rows(vr_list)


# 2009-2012 xls files ----
# requires verifying structure of files are consistent
# and downloading files to folder (via script) ahead of time

# ## Test ----
# # can I read in all of the localities (cell B8)
# sheets <- excel_sheets("datadownloads/vr12.xlsx") # get list of tab names
# 
# localities <- lapply(sheets[-135], function(x) # across all tabs
#   read_excel("datadownloads/vr12.xlsx", range = "b8", # read in locality name cell
#              col_names = FALSE, sheet = x))
# tables <- lapply(sheets[-135], function(x) # across all tabs
#   read_excel("datadownloads/vr12.xlsx", skip = 8, sheet = x)) # read in data table
# tables <- lapply(tables, function(x) {x <- x[1:(nrow(x)-1),]}) # remove last row (totals)
# names(tables) <- unlist(localities[-135]) # assign locality names to list of data tables
# 
# dftest <- bind_rows(tables, .id = "locality") # turn into a data frame; keep locality names

## Make functions ----
# create_voteregtable: create voter registration table
# arguments: file
create_voteregtable <- function(file){
  
  sheets <- excel_sheets(file)
  localities <- lapply(sheets[-135], function(x) read_excel(file, range = "b8", col_names = FALSE, sheet = x))
  tables <- lapply(sheets[-135], function(x) read_excel(file, skip = 8, sheet = x))
  tables <- lapply(tables, function(x) {x <- x[1:(nrow(x)-1),]})
  names(tables) <- unlist(localities[-135])
  
  df <- bind_rows(tables, .id = "Locality")
  
  return(df)
  }

# extract_regvoters2: output selected locality registration totals
# arguments: df, year, localities

extract_regvoters2 <- function(df, year, localities){
  
  loc <- str_flatten(localities, collapse = "|")
  
  df <- df %>% 
    mutate(Locality = str_replace(Locality, "Locality: ", ""),
           fips = str_sub(Locality, 1,3),
           Locality = str_remove_all(Locality, "[0-9]"),
           Locality = str_trim(Locality),
           Locality = str_to_title(Locality)) %>% 
    filter(str_detect(Locality, loc)) %>% 
    group_by(Locality, fips) %>% 
    summarize(voters = sum(All)) %>% 
    mutate(year = year) %>% 
    select(locality = Locality, fips, voters, year) %>% 
    bind_rows(df %>% 
                summarize(voters = sum(All)) %>% 
                mutate(year = year, fips = "51", locality = "Virginia") %>% 
                select(locality, fips, voters, year))
  
  return(df)
}

## download files ----
## years
years <- seq(2009,2012,1)
## urllist
urllist <- paste0("https://www.elections.virginia.gov/media/registration-statistics/",
                  years,
                  "/12/Registrant_Counts_By_Locality.xls")
url_year <- data.frame(url = urllist, year = years) %>% 
  mutate(download = paste0("datadownloads/vr", year, ".xls"))

map2(url_year$url, url_year$download, ~download.file(.x, .y))

## create data tables ----
# 2012 didn't read in initially; opened it and saved as .xlsx 
# (and added new download file name to read)
url_year <- url_year %>% 
  mutate(download2 = ifelse(year == 2012, paste0(download, "x"), download))

tbls <- map(url_year$download2, ~create_voteregtable(.x))

## generate local totals ----
vr0912 <- map2(tbls, url_year$year, 
               ~extract_regvoters2(.x, .y, c("Albemarle", "Charlottesville")))


# 2008 ----
# distinctive spreadsheet, similar to 20013-2021 but with different columns/names
ss08 <- "https://www.elections.virginia.gov/media/registration-statistics/2008/12/rpt_VoterCountsLocalitySummary.xls"
download.file(ss08, "datadownloads/vr2008.xls")
tab08 <- read_xls("datadownloads/vr2008.xls")

vr08 <- tab08 %>% 
  mutate(locality = str_to_title(LocalityName)) %>% 
  filter(str_detect(locality, "Albemarle|Charlottesville")) %>% 
  select(locality, fips = LOCALITY_CODE, ALL_VOTERS) %>% 
  group_by(locality, fips) %>% 
  summarize(voters = sum(ALL_VOTERS)) %>% 
  mutate(year = 2008) %>% 
  select(locality, fips, voters, year) %>% 
  bind_rows(tab08 %>% 
              summarize(voters = sum(ALL_VOTERS)) %>% 
              mutate(year = 2008, fips = "51", locality = "Virginia") %>% 
              select(locality, fips, voters, year))

rm(ss08, tab08)


# 2007 pdfs ----
# distinctive pdf, each locality is separate table (like in 2009-2012)
# but with tables not contained on a single page
# I'm not seeing any easy way of reading all of this in; 
# and as it's the only one like this, just going to brute force it
# and grab the three desired totals

pdf07 <- "https://www.elections.virginia.gov/media/registration-statistics/2007/12/Registrant_Counts_By_Locality.pdf"

tabalb <- extract_tables(pdf07, pages = 3, output = "data.frame")
view(tabalb[[1]])
vr07alb <- tabalb[[1]][nrow(tabalb[[1]]),6]

tabcvl <- extract_tables(pdf07, pages = 129, output = "data.frame")
view(tabcvl[[1]])
vr07cvl <- tabcvl[[1]][nrow(tabcvl[[1]]),6]

tabst <- extract_tables(pdf07, pages = 176, output = "data.frame")
view(tabst[[1]])
vr07st <- tabst[[1]][nrow(tabst[[1]]),6]

# and put them in a dataframe
vr07 <- data.frame(locality = c("Albemarle County", "Charlottesville City", "Virginia"),
                       fips = c("003", "540", "51"),
                       voters = c(vr07alb, vr07cvl, vr07st),
                       year = 2007)
vr07 <- vr07 %>% 
  mutate(voters = as.numeric(gsub(",", "", voters)))

rm(pdf07, tabalb, tabcvl, tabst, vr07alb, vr07cvl, vr07st)


# 2000-2006 pdfs ----
# 2004-2006 operate similarly; 2003 has a column read error; 2000-2002 operate similarly
# started a function, but it's different for 2004-2006, for 2000-2002, and for 2003
# so just repeating it with slight adjustments for each year (for now)

## 2006 ----
pdf06 <- "https://www.elections.virginia.gov/media/registration-statistics/2006/12/200612_precinctsandreg.pdf"
# define area to read; can I use this for all years?
# pdfarea <- locate_areas(pdf06, 1) # selected manually
pdfarea <- list(setNames(c(93, 3, 635, 610), 
                         c("top", "left", "bottom", "right")))
list06 <- extract_tables(pdf06, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list06 <- map(list06, ~mutate_all(., as.character))
view(list06[[2]])
# remove empty rows on top
list06 <- lapply(list06, function(x) {x <- x[3:nrow(x), ]})
# bind these into a single data frame
df06 <- bind_rows(list06)
names(df06) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
# remove empty row before total and name locality for total
df06 <- df06 %>% 
  filter(voters != "") %>% 
  mutate(locality = ifelse(locality == "", "Virginia", locality))

vr06 <- df06 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2006, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year) 

rm(pdf06, list06, df06)

## 2005 ----
pdf05 <- "https://www.elections.virginia.gov/media/registration-statistics/2005/12/200512_precinctsandreg.pdf"
list05 <- extract_tables(pdf05, area = pdfarea, 
                         guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list05 <- map(list05, ~mutate_all(., as.character))
# remove empty rows on top
list05 <- lapply(list05, function(x) {x <- x[3:nrow(x), ]})
# bind these into a single data frame
df05 <- bind_rows(list05)
names(df05) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
# remove empty row before total and name locality for total
df05 <- df05 %>% 
  filter(voters != "") %>% 
  mutate(locality = ifelse(locality == "Statewide Totals:", "Virginia", locality))

vr05 <- df05 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2005, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf05, list05, df05)

## 2004 ----
pdf04 <- "https://www.elections.virginia.gov/media/registration-statistics/2004/12/200412_precinctsandreg.pdf"
list04 <- extract_tables(pdf04, area = pdfarea, 
                         guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list04 <- map(list04, ~mutate_all(., as.character))
# remove empty rows on top
list04 <- lapply(list04, function(x) {x <- x[3:nrow(x), ]})
# bind these into a single data frame
df04 <- bind_rows(list04)
names(df04) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
# remove empty row before total and name locality for total
df04 <- df04 %>% 
  filter(voters != "") %>% 
  mutate(locality = ifelse(locality == "Statewide Totals:", "Virginia", locality))

vr04 <- df04 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2004, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf04, list04, df04)

## 2003 ----
pdf03 <- "https://www.elections.virginia.gov/media/registration-statistics/2003/12/200312_precinctsandreg.pdf"
list03 <- extract_tables(pdf03, area = pdfarea, 
                         guess = FALSE, output = "data.frame")
# pages 2-3 concatenate last two columns; will need to treat page 1 and others separately
# turn everything into a character (to use bind_rows)
list03 <- map(list03, ~mutate_all(., as.character))

df03a <- list03[[1]]
names(df03a) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
df03b <- bind_rows(list03[2:4])
names(df03b) <- c("locality", "precinct", "active", "inactive", "overseas voters")

# separate overseas and voters in df03b and name locality for total
df03b <- df03b %>% 
  filter(locality != "") %>% 
  separate(`overseas voters`, c("overseas", "voters"), sep = " ") %>% 
  mutate(locality = ifelse(locality == "State totals:", "Virginia", locality))

df03 <- bind_rows(df03a, df03b)

vr03 <- df03 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2003, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf03, list03, df03a, df03b, df03)

## 2002 ----
pdf02 <- "https://www.elections.virginia.gov/media/registration-statistics/2002/Voters.by.Loc.dec02.pdf"
list02 <- extract_tables(pdf02, area = pdfarea, 
                         guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list02 <- map(list02, ~mutate_all(., as.character))

df02 <- bind_rows(list02) # seems like too few rows? Some late alphabet cities are missing...
names(df02) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
df02 <- df02 %>% 
  mutate(locality = ifelse(locality == "State totals:", "Virginia", locality))

vr02 <- df02 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2002, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf02, list02, df02)

## 2001 ----
pdf01 <- "https://www.elections.virginia.gov/media/registration-statistics/2001/Voters.by.loc.dec01.PDF"
list01 <- extract_tables(pdf01, area = pdfarea, 
                         guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list01 <- map(list01, ~mutate_all(., as.character))

df01 <- bind_rows(list01) 
names(df01) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
df01 <- df01 %>% 
  mutate(locality = ifelse(locality == "State totals:", "Virginia", locality))

vr01 <- df01 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2001, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf01, list01, df01)

## 2000 ----
pdf00 <- "https://www.elections.virginia.gov/media/registration-statistics/2000/Voters.by.loc.dec00.PDF"
list00 <- extract_tables(pdf00, area = pdfarea, 
                         guess = FALSE, output = "data.frame")

# turn everything into a character (to use bind_rows)
list00 <- map(list00, ~mutate_all(., as.character))

df00 <- bind_rows(list00) 
names(df00) <- c("locality", "precinct", "active", "inactive", "overseas", "voters")
df00 <- df00 %>% 
  mutate(locality = ifelse(locality == "State totals:", "Virginia", locality))

vr00 <- df00 %>% 
  filter(locality %in% c("Albemarle County", "Charlottesville City", "Virginia")) %>% 
  mutate(year = 2000, voters = as.numeric(gsub(",", "", voters)),
         fips = case_when(locality == "Albemarle County" ~ "003",
                          locality == "Charlottesville City" ~ "540",
                          TRUE ~ "51")) %>% 
  select(locality, fips, voters, year)

rm(pdf00, list00, df00)


# Combine years ----
df_list <- mget(ls(pattern = "vr[0-9]"))
votereg <- bind_rows(df_list)


# Add population for rate ----
pop <- read_csv("data/pop_data_cdc.csv")

pop <- pop %>% 
  mutate(fips_county = str_sub(fips, 3,5),
         fips_county = ifelse(fips_county == "", "51", fips_county))

votereg <- votereg %>% 
  left_join(pop %>% select(year, pop_18over, fips_county),
            by = c("year" = "year", "fips" = "fips_county"))

## fill in 2021 population with 2020 estimates
votereg <- votereg %>%
  group_by(locality, fips) %>%
  fill(pop_18over) %>%
  ungroup()

## Create registration rate 
votereg <- votereg %>% 
  mutate(reg_rate = voters/pop_18over)

## Have a peek 
summary(votereg)

votereg %>% 
  group_by(fips) %>% 
  summarize(mean(reg_rate))

ggplot(votereg, aes(x = year, y = reg_rate, color = locality)) +
  geom_line()


# Save ----
write_csv(votereg, "data/vote_reg.csv")
# votereg <- read_csv("data/vote_reg.csv")
