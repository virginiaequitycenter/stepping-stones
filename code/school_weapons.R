# Stepping Stones Data: Weapons Offenses in Schools ----
# Updated 2023-02-18
# Contributor: Michele Claibourn
# Acquire data from Discipline, Crime, and Violence Reports
# https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# and from Safe Schools Information Resource: https://p1pe.doe.virginia.gov/pti/selection.do
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Education, "Discipline, Crime, and Violence Reports" 2006-2020.
# https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education


# Libraries ----
library(tidyverse)
library(tabulizer) 
library(janitor)
library(here)
library(readxl)


# PDFs from DCV reports ----
## 2012-2013 ----
# Find the "pdfarea" - the area on the page that you want to extract
# requires knowing the page that the table is on beforehand

# For 2012-2013 report: pages 68 & 69
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20775/638043641278270000"
# area <- locate_areas(url, pages = c(68,69)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(100, 30, 435, 770), 
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(68,69), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "aggbat", "alc", "alter", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "defi", "miscsplit", "extort", "fight", "gamble", 
             "gang", "gun", "harrass", "haze", "homi", "inapp", "riot", "kidnap", "wound")
             
# combined: "demos", "dis", "drug" (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "inhale", "insubord", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "razor", "taser", "toygun", "fireworks", "otc", "robbery", 
             "miscsplit2", "sxharass", "sxoffense", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined: "threat", "sxass", "sxbatt" (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(aggbat))
t2 <- t2 %>% filter(!is.na(inhale))

t <- bind_cols(t1, t2)

# filter to localities of interest
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2013 <- t %>% 
  filter(str_detect(division2, "Charlottesville|Albemarle|STATE")) %>% 
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = str_extract(division2, "Charlottesville|Albemarle|STATE"),
         division = ifelse(division == "STATE", "Virginia", division),
         school_year = "2012-2013") %>% 
  select(division, school_year, weapon_count)


## 2011-2012 ----
# For 2011-2012 report: pages 66 & 67
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20773/638043641269370000"
# area <- locate_areas(url, pages = c(66,67)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(93, 45, 550, 760), 
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(66,67), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "blank1", "aggbat", "alc", "alter", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "miscsplit", "extort", "fight", "gamble", 
             "gang", "gun", "harrass", "haze", "homi", "inapp", "riot", "kidnap", "wound")

# combined: "defi", "demos", "dis", "drug" (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "blank2", "inhale", "insubord", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "razor", "taser", "toygun", 
             "miscsplit2", "sxharass", "sxoffense", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined: "fireworks", "otc", "robbery", "threat", "sxass", "sxbatt" (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(aggbat))
t2 <- t2 %>% filter(!is.na(inhale))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2012 <- t %>% 
  filter(str_detect(division2, "CHAR|ALBEMARLE")) %>% 
  mutate(fireworks =  str_extract(miscsplit2, "^[0-9]")) %>% # extract fireworks column, combined in miscplit2
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = ifelse(str_detect(division2, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2011-2012") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 28)
area <- list(setNames(c(96, 65, 420, 769), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 28, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(X == "Total") %>% 
  select(division = X, weapon_count = X.1) %>% 
  mutate(school_year = "2011-2012",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2012 <- bind_rows(df2012, t)


## 2010-2011 ----
# For 2010-2011 report: pages 67 & 68
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20771/638043641256200000"
# area <- locate_areas(url, pages = c(67,68)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(100, 50, 690, 760), 
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(67,68), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "alc", "alter", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "defi", "miscsplit", "drug", 
             "extort", "fight", "blank2", "blank3", "gamble", "gang", "gun", "harrass", "haze", 
             "homi", "inapp", "riot", "kidnap", "wound", "blank4")

# combined: "aggbat" in division; "demos", "dis" (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "inhale", "insubord", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "blank5", "razor", "taser", "toygun", "fireworks", 
             "miscsplit2", "sxharass", "sxoffense", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined: "otc", "robbery", "threat", "sxass", "sxbatt" (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(alc))
t2 <- t2 %>% filter(!is.na(inhale))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2011 <- t %>% 
  filter(str_detect(division1, "CHARLOTTESVILLE|ALBEMARLE")) %>% 
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2010-2011") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 29)
area <- list(setNames(c(142, 65, 460, 560), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 29, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(Offenses == "TOTAL") %>% 
  select(division = Offenses, weapon_count = Count) %>% 
  mutate(school_year = "2010-2011",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2011 <- bind_rows(df2011, t)


## 2009-2010 ----
# For 2009-2010 report: pages 67 & 68
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20769/638043641244630000"
# area <- locate_areas(url, pages = c(67,68)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(112, 50, 700, 750), 
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(67,68), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "alc", "alter", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "miscsplit", "drug", "blank1",
             "extort", "blank2", "fight", "gamble", "gang", "gun", "harrass", "haze", 
             "homi", "inapp", "riot", "kidnap", "wound", "blank3", "blank4")

# combined: "aggbat" in division; "defi", "demos", "dis" (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "blank5", "inhale", "insubord", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "razor", "taser", "toygun", "fireworks", 
             "miscsplit2", "blank6", "blank7", "sxbatt", "sxharass", "sxoffense", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined: "otc", "robbery", "threat", "sxass",  (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(alc != "") %>% filter(str_detect(division1, "ALBEMARLE|CHARLOTTESVILLE"))
t2 <- t2 %>% filter(!is.na(inhale)) %>% filter(str_detect(division2, "ALBEM|CHARL"))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2010 <- t %>% 
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2009-2010") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 29)
area <- list(setNames(c(110, 65, 450, 560), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 29, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(X == "TOTAL") %>% 
  select(division = X, weapon_count = X.1) %>% 
  mutate(school_year = "2009-2010",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2010 <- bind_rows(df2010, t)


## 2008-2009 ----
# For 2008-2009 report: pages 63 & 64
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20767/638043641238230000"
# area <- locate_areas(url, pages = c(63,64)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(100, 30, 570, 730), 
                      c("top", "left", "bottom", "right")),
             setNames(c(80, 35, 550, 750),
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(63,64), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "misc1", "misc2", "blank1", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "defi", "miscsplit", "fight",
              "gamble", "gang", "gun", "harrass", "haze", "homi", "inapp", "riot", "kidnap")

# combined: "aggbat", "alc", "alter" in division/misc; "demos", "dis", "drug", "extort",  (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "wound", "insubord", "blank1", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "taser", "razor", "toygun", "fireworks", "otc", "robbery",
             "miscsplit2", "blank2", "blank3", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined: "threat", "sxass", sxbatt", "sxharass", "sxoffense", (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(arson)) %>% filter(division1 %in% c("ALBEMARLE CO.", "CITY"))
t2 <- t2 %>% filter(!is.na(wound)) %>% filter(str_detect(division2, "ALBE|CHARL"))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2009 <- t %>% 
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2008-2009") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 25)
area <- list(setNames(c(90, 42, 376, 555), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 25, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(X == "TOTAL") %>% 
  select(division = X, weapon_count = X.1) %>% 
  mutate(school_year = "2008-2009",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2009 <- bind_rows(df2009, t)


## 2007-2008 ----
# For 2008-2009 report: pages 67 & 68
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20765/638043641231200000"
# area <- locate_areas(url, pages = c(67,68)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(88, 30, 550, 730), 
                      c("top", "left", "bottom", "right")),
             setNames(c(88, 35, 550, 750),
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(67,68), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "aggbat", "misc1", "arson", "att", "staffbat", "studbat", "beep", "bande", 
             "tob", "bully", "comp", "cell", "disrupt", "defi", "miscsplit", "fight",
             "gamble", "gang", "gun", "harrass", "haze", "homi", "inapp", "riot", "kidnap")

# combined: "alc", "alter" in division/misc; "demos", "dis", "drug", "extort",  (these were collapsed into a single column, along with column headers)
names(t1) <- t1names

t2names <- c("division2", "blank1", "wound", "insubord", "misrep", "lang", "lit", "touch", "device", 
             "violence", "weapons", "taser", "razor", "toygun", "fireworks", "otc", "robbery",
             "miscsplit2", "sxoffense", "blank3", "stalk", "theft", "threat2", 
             "tob2", "trespass", "tech", "vandal", "use", "internet", "total")
# combined:  "threat", "sxass", sxbatt", "sxharass",  (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(arson)) %>% filter(division1 %in% c("ALBEMARLE CO.", "CITY"))
t2 <- t2 %>% filter(!is.na(wound)) %>% filter(str_detect(division2, "ALBE|CHARL"))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2008 <- t %>% 
  select(division1, division2, gun, weapons, razor, taser, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+taser+toygun+fireworks,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2007-2008") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 30)
area <- list(setNames(c(460, 55, 710, 560), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 30, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(Offenses == "Total") %>% 
  select(division = Offenses, weapon_count = Count) %>% 
  mutate(school_year = "2007-2008",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2008 <- bind_rows(df2008, t)


## 2006-2007 ----
# For 2006-2007 report: pages 77 & 78
url <- "https://www.doe.virginia.gov/home/showpublisheddocument/20761/638043641222770000"
# area <- locate_areas(url, pages = c(77,78)) # find area in document manually
# area # print to console and reset by hand (for reproducibility; choosing outer value for each dimension)
area <- list(setNames(c(90, 30, 550, 730), 
                      c("top", "left", "bottom", "right")),
             setNames(c(100, 30, 580, 750),
                      c("top", "left", "bottom", "right")))

tables <- extract_tables(url, pages = c(77,78), area = area, # extract tables from pdf into list of data frames
                         guess = FALSE, output = "data.frame")

t1 <- tables[[1]] # put each table into separate data frame
t2 <- tables[[2]]

# rename columns manually
t1names <- c("division1", "alc", "arson", "staffbat", "studbat", "wound", "threat1", "bande",
             "bully", "disorder", "drugs", "extort", "fight1", "fight2", "gang", "homi", 
              "misc1", "blank1", "sxbatt", "aggbat", "tob1", "theft", "threat", 
             "trespass", "vandal", "gun", "weapons")

names(t1) <- t1names

t2names <- c("blank2", "division2", "cell", "device", "dis", "defi", "lit", "otc", "disrupt", 
             "otc2", "lang", "otc3", "insubord", "alter", "gamble", "misc2",
             "touch3", "blank3", "viol", "tech", "comp", "use", "tob2", "internet", 
             "toygun", "razor",  "fireworks", "total")
# beep, att part of division
# combined:  "haze", "inapp", "touch", "misrep" (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(arson)) %>% filter(division1 %in% c("ALBEMARLE CO", "CITY"))
t2 <- t2 %>% filter(!is.na(cell)) %>% filter(str_detect(division2, "ALBE|VILLE"))

t <- bind_cols(t1, t2)

# filter to localities of interest (can't recover state totals from this table)
# and select weapons-related columns (gun, weapons, razor, taser, toygun, fireworks) 
# remove the commas from some count values and then sum (and fix division names)
df2007 <- t %>% 
  select(division1, division2, gun, weapons, razor, toygun, fireworks) %>% 
  mutate(across(gun:fireworks, as.character),
         across(gun:fireworks, ~str_remove(.x, ",")),
         across(gun:fireworks, as.numeric),
         weapon_count = gun+weapons+razor+toygun+fireworks,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2006-2007") %>% 
  select(division, school_year, weapon_count)

# get state totals from different table and append
# area <- locate_areas(url, page = 27)
area <- list(setNames(c(118, 83, 317, 530), 
                      c("top", "left", "bottom", "right")))
tables <- extract_tables(url, page = 27, area = area, 
                         guess = FALSE, output = "data.frame")
t <- tables[[1]]

t <- t %>% 
  filter(X == "") %>% 
  select(division = X, weapon_count = X.1) %>% 
  mutate(school_year = "2006-2007",
         division = "Virginia",
         weapon_count = str_remove(weapon_count, ","),
         weapon_count = as.numeric(weapon_count))

# Bind state to localities
df2007 <- bind_rows(df2007, t)


## Bind all years together ----
df <- bind_rows(df2007, df2008, df2009, df2010, df2011, df2012, df2013)


# From SSIR ---- 
## 2016-2017 ----
# Select 2016-2017
# Select Offense Category = Weapons Offenses
# Select All for everything else
# Downlaod Offense Frequency report

data2016 <- read_csv("datadownloads/Offense Frequency Report.csv", skip = 16) %>% 
  clean_names()

df2016 <- data2016 %>% 
  filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
  select(division = division_name, school_year, weapon_count = related_to_weapons) %>% 
  mutate(weapon_count = as.numeric(weapon_count),
         division = ifelse(division == "Albemarle County Public Schools",
                       "Albemarle", "Charlottesville"),
         school_year = "2016-2017")

# create state total
state2016 <- data2016 %>% 
  mutate(weapons = as.numeric(related_to_weapons)) 
# fill in suppressed values with random draw between 1 and 5 
# suppression rule is anything under 10, so theoretically, this is still an undercount
set.seed(121)
state2016$weapons[is.na(state2016$weapons)] <- round(runif(n = length(which(is.na(state2016$weapons))), min=1, max=5),0)

# now create a state total
state2016 <- state2016 %>% 
  summarize(weapon_count = sum(weapons)) %>% 
  mutate(school_year = "2016-2017",
         division = "Virginia")

# bind state to localities
df2016 <- bind_rows(df2016, state2016)


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
dcv_cvlalb <- dcv_data %>% 
  filter(division_number %in% c(2,104),
         offense_category == 29) %>% 
  select(school_year, division = division_name, weapon_count = count_of_incidents) %>% 
  mutate(division = ifelse(division == "Albemarle County", "Albemarle", "Charlottesville"))
# charlottesville not present in 2019, 2021; 
# rows with offense codes appear to only be present when offense is present

dcv_data %>% filter(offense_category == 29) %>% 
  ggplot(aes(x = count_of_incidents)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 10)
# but not censored; so we can assume absence is zero

# fill in missing Cville rows
dcv_cvlalb <- dcv_cvlalb %>% 
  mutate(year = str_sub(school_year, 6,9),
         year = as.numeric(year)) %>% 
  complete(division, year = 2018:2021,
           fill = list(weapon_count = 0)) %>% 
  mutate(school_year = ifelse(is.na(school_year), 
                              paste0(year-1, "-", year),
                              school_year)) %>% 
  select(-year)
  
## create state totals ----
dcv_va <- dcv_data %>% 
  filter(offense_category == 29) %>% 
  group_by(school_year) %>% 
  summarize(weapon_count = sum(count_of_incidents)) %>% 
  mutate(division = "Virginia")

## bind state and localities ----
df1821 <- bind_rows(dcv_cvlalb, dcv_va)


# Bind all years of data ----
# df <- read_csv("data/school_weapons_dcv.csv")
# df <- bind_rows(df, df1821)
df <- bind_rows(df, df2016, df1821)

# fill in missing years
df_complete <- df %>% 
  mutate(year = str_sub(school_year, 6,9),
         year = as.numeric(year)) %>% 
  complete(division, year = 2007:2021,
           fill = list(weapon_count = NA_real_)) %>% 
  mutate(school_year = ifelse(is.na(school_year), 
                              paste0(year-1, "-", year),
                              school_year)) 

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
students <- bind_rows(stud_cvlalb, stud_va)


# Join weapons offense and student counts ----
df_students <- df_complete %>% 
  left_join(students)
# and generate the rate 
df_students <- df_students %>% 
  mutate(rate = (weapon_count/students)*1000)

# have a peek
ggplot(df_students, aes(x = year, y = rate, color = division, group = division)) + geom_line()


# Save data ----
write_csv(df_students, "data/school_weapons_dcv.csv")


