# Stepping Stones Data: Violent Offenses in Schools ----
# Updated 2023-02-21
# Contributor: Michele Claibourn
# Acquire data from Discipline, Crime, and Violence Reports
# https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# and from Safe Schools Information Resource: https://p1pe.doe.virginia.gov/pti/selection.do
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Education, "Discipline, Crime, and Violence Reports" 2006-2021.
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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(miscsplit = str_replace_all(miscsplit, "[:alpha:]", "")) %>% 
  separate(miscsplit, into=c('demos', 'dis', 'drug'), sep="\\s+")

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c('threat', 'sxass', 'sxbatt'))

# filter to localities of interest
# and select violence-related columns - those identified in city spreadsheet, excluding gun and weapons
# (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2013 <- tclean %>% 
  filter(str_detect(division2, "Charlottesville|Albemarle|STATE")) %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = str_extract(division2, "Charlottesville|Albemarle|STATE"),
         division = ifelse(division == "STATE", "Virginia", division),
         school_year = "2012-2013") %>% 
  select(division, school_year, aggbat:count)


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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(miscsplit = str_replace_all(miscsplit, "[:alpha:]", "")) %>% 
  separate(miscsplit, into=c("defi", "demos", "dis", "drug"), sep="\\s+")
# for some rows, the collapse occured in the disrupt column (but not cvl/alb)

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c("fireworks", "otc", "robbery", "threat", "sxass", "sxbatt"))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2012 <- tclean %>% 
  filter(str_detect(division2, "CHAR|ALBEMARLE")) %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division2, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2011-2012") %>% 
  select(division, school_year, aggbat:count)

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabularizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(4), staffbat = c(1178), studbat = c(3922),
                 fight = c(7279), homi = c(0), kidnap = c(0), 
                 wound = c(24), robbery = c(13), sxass = c(1),
                 sxbatt = c(23), sxoffense = c(361), threat2 = c(6432)
)

t <- t %>% 
  mutate(school_year = "2011-2012",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2)

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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(miscsplit = str_replace_all(miscsplit, "[:alpha:]", "")) %>% 
  separate(miscsplit, into=c("demos", "dis"), sep="\\s+")
# for some rows, the collapse occurred in the disrupt column (but not cvl/alb)

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c("otc", "robbery", "threat", "sxass", "sxbatt"))

tclean <- tclean %>% 
  mutate(aggbat = str_sub(division1, -1))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2011 <- tclean %>% 
  filter(str_detect(division1, "CHARLOTTESVILLE|ALBEMARLE")) %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2010-2011") %>% 
  select(division, school_year, aggbat:count)

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabulizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(1), staffbat = c(1209), studbat = c(3829),
                 fight = c(7215), homi = c(0), kidnap = c(0), 
                 wound = c(24), robbery = c(28), sxass = c(1),
                 sxbatt = c(40), sxoffense = c(355), threat2 = c(6368)
)

t <- t %>% 
  mutate(school_year = "2010-2011",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2)

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
             "tob", "bully", "comp", "cell", "disrupt", "miscsplit", "dis", "drug", 
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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(miscsplit = str_replace_all(miscsplit, "[:alpha:]", "")) %>% 
  separate(miscsplit, into=c("defi", "demos"), sep="\\s+")
# for some rows, the collapse occurred in the disrupt column (but not cvl/alb)

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c("otc", "robbery", "threat", "sxass"))

tclean <- tclean %>% 
  mutate(aggbat = str_sub(division1, -1))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2010 <- tclean %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2009-2010") %>% 
  select(division, school_year, aggbat:count)

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabulizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(2), staffbat = c(1251), studbat = c(4235),
                 fight = c(7806), homi = c(0), kidnap = c(0), 
                 wound = c(34), robbery = c(16), sxass = c(2),
                 sxbatt = c(44), sxoffense = c(385), threat2 = c(6189)
)

t <- t %>% 
  mutate(school_year = "2009-2010",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2)

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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(aggbat = str_sub(misc1, -1),
         alc = str_sub(misc2, 1,1),
         alter = str_extract(misc2, "[0-9]+$"))

tclean <- tclean %>% 
  mutate(miscsplit = str_replace_all(miscsplit, "[:alpha:]", "")) %>% 
  separate(miscsplit, into=c("demos", "dis", "drug", "extort"), sep="\\s+") %>% 
  mutate(dis = ifelse(division1 == "ALBEMARLE CO.", 194, dis), # manually correct
         drug = ifelse(division1 == "ALBEMARLE CO.", 28, 18),
         extort = 0)

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c("threat", "sxass", "sxbatt", "sxharass", "sxoffense"))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2009 <- tclean %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2008-2009") %>% 
  select(division, school_year, aggbat:count)

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabulizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(2), staffbat = c(1275), studbat = c(4381),
                 fight = c(8624), homi = c(0), kidnap = c(1), 
                 wound = c(31), robbery = c(36), sxass = c(0),
                 sxbatt = c(26), sxoffense = c(374), threat2 = c(6349)
)

t <- t %>% 
  mutate(school_year = "2008-2009",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2)

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

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  separate(misc1, into=c("alc", "alter"), sep="\\s+") 

tclean <- tclean %>% 
  separate(miscsplit, into=c("demos", "dis", "drug", "extort"), sep="\\s+") 

tclean <- tclean %>% 
  mutate(miscsplit2 = str_replace_all(miscsplit2, "[:alpha:]", "")) %>% 
  separate(miscsplit2, into=c("threat", "sxass", "sxbatt", "sxharass"))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2008 <- tclean %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2007-2008") %>% 
  select(division, school_year, aggbat:count)

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabulizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(4), staffbat = c(1393), studbat = c(4657),
                 fight = c(9327), homi = c(0), kidnap = c(1), 
                 wound = c(34), robbery = c(36), sxass = c(1),
                 sxbatt = c(28), sxoffense = c(390), threat2 = c(6789)
)

t <- t %>% 
  mutate(school_year = "2007-2008",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxass+sxbatt+sxoffense+threat2)

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
t1names <- c("division1", "alc", "arson", "staffbat", "studbat", "wound", "threat", "bande",
             "bully", "disorder", "drugs", "extort", "fight1", "fight2", "gang", "homi", 
              "misc1", "blank1", "sxbatt", "aggbat", "tob1", "theft", "threat2", 
             "trespass", "vandal", "gun", "weapons")

names(t1) <- t1names

t2names <- c("blank2", "division2", "cell", "device", "dis", "defi", "lit", "otc", "disrupt", 
             "otc2", "lang", "otc3", "insubord", "alter", "gamble", "misc2",
             "touch2", "blank3", "viol", "tech", "comp", "use", "tob2", "internet", 
             "toygun", "razor",  "fireworks", "total")
# beep, att part of division
# combined:  "haze", "inapp", "touch", "misrep" (these were collapsed into a single column, along with column headers)
names(t2) <- t2names

# remove empty rows from each and column bind the two together
t1 <- t1 %>% filter(!is.na(arson)) %>% filter(division1 %in% c("ALBEMARLE CO", "CITY"))
t2 <- t2 %>% filter(!is.na(cell)) %>% filter(str_detect(division2, "ALBE|VILLE"))

t <- bind_cols(t1, t2)

# split combined columns to retrieve collapsed vars
# remove letters from misc columns
tclean <- t %>% 
  mutate(alc = ifelse(division1 == "CITY", str_sub(alc, 1,2), alc),
         staffbat = ifelse(division1 == "CITY", str_sub(staffbat, -1), staffbat))

tclean <- tclean %>% 
  mutate(misc1 = str_replace_all(misc1, "[:alpha:]", "")) %>% 
  separate(misc1, into=c("kidnap", "robbery", "riot", "stalk", "sxoffense"), sep="\\s+") 

tclean <- tclean %>% 
  mutate(misc2 = str_replace_all(misc2, "[:alpha:]", "")) %>% 
  separate(misc2, into=c("haze", "inapp", "touch1", "misrep")) %>% 
  mutate(touch2 = ifelse(division1 == "CITY", 2, touch2))

# filter to localities of interest (can't recover state totals from this table)
# and select violence-related columns (aggbat, staffbat, studbat, fight, homi, kidnap, wound, robbery, sxass, sxbatt, sxoffense, threat2) 
# remove the commas from some count values and then sum (and fix division names)
df2007 <- tclean %>% 
  select(division1, division2, aggbat, staffbat, studbat, fight1, fight2, homi, kidnap, wound, robbery, sxbatt, sxoffense, threat2) %>% 
  mutate(across(aggbat:threat2, as.character),
         across(aggbat:threat2, ~str_remove(.x, ",")),
         across(aggbat:threat2, as.numeric),
         fight = fight1+fight2,
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxbatt+sxoffense+threat2,
         division = ifelse(str_detect(division1, "ALBEMARLE"), "Albemarle", "Charlottesville"),
         school_year = "2006-2007") %>% 
  select(division, school_year, aggbat:count, -c(fight1, fight2))

# write state totals and append
# the state totals in this table are printed vertically
# and show up in inconsistent ways in the table as read in
# by tabulizer -- adding the counts by head in the interim
t <- data.frame (aggbat  = c(7), staffbat = c(1523), studbat = c(6650),
                 fight = c(11073), homi = c(0), kidnap = c(0), 
                 wound = c(39), robbery = c(31),
                 sxbatt = c(46), sxoffense = c(1515), threat2 = c(8501)
)

t <- t %>% 
  mutate(school_year = "2006-2007",
         division = "Virginia",
         count = aggbat+staffbat+studbat+fight+homi+kidnap+wound+robbery+sxbatt+sxoffense+threat2)

# Bind state to localities
df2007 <- bind_rows(df2007, t)


## Bind all years together ----
df <- bind_rows(df2007, df2008, df2009, df2010, df2011, df2012, df2013)


# # From SSIR ---- 
# ## 2016-2017 ----
# # Select 2016-2017
# # Select Offense Category = Offenses against Students, Offenses against Staff
# # and Offenses against Other Persons (selection)
# # Select All for everything else
# # Downlaod Offense Frequency report
# 
# d1 <- read_csv("datadownloads/Offense Frequency Report-student.csv", skip = 16) %>% 
#   clean_names()
# d2 <- read_csv("datadownloads/Offense Frequency Report-staff.csv", skip = 16) %>% 
#   clean_names()
# d3 <- read_csv("datadownloads/Offense Frequency Report-otherperson.csv", skip = 16) %>% 
#   clean_names()
# d4 <- read_csv("datadownloads/Offense Frequency Report-other-sxoffense.csv", skip = 16) %>% 
#   clean_names()
# 
# d1cvlalb <- d1 %>% 
#   filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
#   select(division = division_name, school_year, count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count),
#          division = ifelse(division == "Albemarle County Public Schools",
#                        "Albemarle", "Charlottesville"),
#          school_year = "2016-2017")
# 
# d2cvlalb <- d2 %>% 
#   filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
#   select(division = division_name, school_year, count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count),
#          division = ifelse(division == "Albemarle County Public Schools",
#                            "Albemarle", "Charlottesville"),
#          school_year = "2016-2017")
# 
# d3cvlalb <- d3 %>% 
#   filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
#   select(division = division_name, school_year, count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count),
#          division = ifelse(division == "Albemarle County Public Schools",
#                            "Albemarle", "Charlottesville"),
#          school_year = "2016-2017")
# 
# d4cvlalb <- d4 %>% 
#   filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
#   select(division = division_name, school_year, count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count),
#          division = ifelse(division == "Albemarle County Public Schools",
#                            "Albemarle", "Charlottesville"),
#          school_year = "2016-2017")
# 
# cvlalb <- bind_rows(d1cvlalb, d2cvlalb, d3cvlalb, d4cvlalb)
# cvlalb <- cvlalb %>% 
#   group_by(division, school_year) %>% 
#   summarize(count = sum(count, na.rm = TRUE))
# 
# # create state total
# d1state <- d1 %>% 
#   select(count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count)) %>% 
#   summarize(count = sum(count, na.rm = TRUE)) %>% 
#   mutate(division = "Virginia", school_year = "2016-2017")
# 
# d2state <- d2 %>% 
#   select(count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count)) %>% 
#   summarize(count = sum(count, na.rm = TRUE)) %>% 
#   mutate(division = "Virginia", school_year = "2016-2017")
# 
# d3state <- d3 %>% 
#   select(count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count)) %>% 
#   summarize(count = sum(count, na.rm = TRUE)) %>% 
#   mutate(division = "Virginia", school_year = "2016-2017")
# 
# d4state <- d4 %>% 
#   select(count = individual_student_offenders) %>% 
#   mutate(count = as.numeric(count)) %>% 
#   summarize(count = sum(count, na.rm = TRUE)) %>% 
#   mutate(division = "Virginia", school_year = "2016-2017")
# 
# state <- bind_rows(d1state, d2state, d3state, d4state)
# state <- state %>% 
#   group_by(division, school_year) %>% 
#   summarize(count = sum(count, na.rm = TRUE))
# 
# # bind state to localities
# df2016 <- bind_rows(cvlalb, state)
# # these numbers appear sufficiently different to not be comparable


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

# find relevant offense categories
dcv_data %>%
  group_by(offense_category_description, offense_category) %>% 
  summarize(n = n()) %>% 
  view()
# Choosing: Assault/Battery, Fighting/Conflict, Kidnapping, Robbery/Person/Force or Threat of Force,
#   Sexual Offenses, Threats/Verbal/Physical
# This set is trying to replicate the categories on the city's spreadsheet
# but I'm not sure these would be the categories I'd select from the start...

## filter and prep ----
dcv_cvlalb <- dcv_data %>% 
  filter(division_number %in% c(2,104),
         offense_category %in% c(3,12,18,20,22,24)) %>% 
  mutate(division = ifelse(division_name == "Albemarle County", "Albemarle", "Charlottesville"))

# pivot to match prior data frame
dcv_wide <- dcv_cvlalb %>% 
  select(-c("region", "offense_category", "division_number", "division_name")) %>% 
  pivot_wider(names_from = offense_category_description, values_from = count_of_incidents) %>% 
  clean_names()

## create state totals ----
dcv_va <- dcv_data %>% 
  filter(offense_category %in% c(3,12,18,20,22,24)) %>% 
  select(school_year, offense_category_description, count_of_incidents) %>% 
  group_by(school_year, offense_category_description) %>% 
  summarize(count = sum(count_of_incidents)) %>% 
  mutate(division = "Virginia")

# pivot wider
dcv_va_wide <- dcv_va %>% 
  pivot_wider(names_from = offense_category_description, values_from = count) %>% 
  clean_names()

## bind state and localities ----
df1821 <- bind_rows(dcv_wide, dcv_va_wide) %>% 
  mutate(source = "new")

# fill in zeroes
df1821 <- df1821 %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


# Bind all years of data ----

# combine/sum prior data (df) to match 18-21 categories
# assault_battery = staffbat, studbat, homi, wound
# fighting_conflict = fight
# sexual_offenses = aggbat, sxass, sxbatt, sxoffense
# threats_verbal_physical = threat2
# kidnapping = kidnap
# robbery = robbery

dfcombined <- df %>% 
  mutate(assault_battery = staffbat+studbat+homi+wound,
         fighting_conflict = fight,
         sexual_offenses = aggbat+sxass+sxbatt+sxoffense,
         threats = threat2,
         kidnapping = kidnap,
         source = "old") %>% 
  select(division, school_year, assault_battery, fighting_conflict, 
         sexual_offenses, threats, kidnapping, robbery, source)

# rename 18-21 data
df1821 <- df1821 %>% 
  rename(robbery = robbery_person_force_or_threat_of_force, threats = threats_verbal_physical)

# bind together
dfcombined <- bind_rows(dfcombined, df1821)

dfcombined <- dfcombined %>% 
  mutate(year = str_sub(school_year, 6,9),
         year = as.numeric(year))

# compare!
ggplot(dfcombined, aes(x = year, y = assault_battery, color = division, group = source)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  theme_bw()

ggplot(dfcombined, aes(x = year, y = fighting_conflict, color = division, group = source)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  theme_bw()

ggplot(dfcombined, aes(x = year, y = sexual_offenses, color = division, group = source)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  theme_bw()

ggplot(dfcombined, aes(x = year, y = threats, color = division, group = source)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  theme_bw()

# Make totals ----
dfcombined <- dfcombined %>% 
  mutate(sexual_offenses = replace_na(sexual_offenses, 0),
         count = assault_battery + fighting_conflict + sexual_offenses +
           threats + kidnapping + robbery)

ggplot(dfcombined, aes(x = year, y = count, color = division, group = source)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  theme_bw()

# add population data ----
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
dfcombined_students <- dfcombined %>% 
  left_join(students)

## Create rate 
dfcombined_students <- dfcombined_students %>% 
  mutate(rate = (count/students)*1000)

ggplot(dfcombined_students, aes(x = year, y = rate, color = division, group = source)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(2007:2013, 2018:2021)) +
  facet_wrap(~division, scales = "free_y", ncol = 1) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw()


# Save data ----
write_csv(dfcombined_students, "data/school_violence_byoffense_dcv.csv")
# dfcombined <- read_csv("data/school_violence_byoffense_dcv.csv")
