# Stepping Stones Data: Divorces ----
# Updated 2023-04-28
# Contributor: Michele Claibourn
# Acquire data from Virginia's Judicial System Caseload Statistical Information
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Judicial System, Caseload Statistical Information, "Caseload Statistics of the Circuit Courts." 2014-2022.
# https://www.vacourts.gov/courtadmin/aoc/judpln/csi/home.html

# consider 2000-2018 vdh data
# https://apps.vdh.virginia.gov/HealthStats/stats.htm


# Libraries ----
library(tidyverse)
library(tabulizer)


# Read in a single pdf report ----
## 2022 ----
# ccurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2022/ccms_1011_dec.pdf"
ccurl22 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2022/ccms_1011_dec.pdf"

# define area to read
# pdf22area <- locate_areas(ccurl22, 1) # selected manually
pdfarea <- list(setNames(c(310, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# virginia
cc22 <- extract_tables(ccurl22, pages = 1, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc22df <- cc22[[1]]
cc22va <- cc22df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(9,11,45,46)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Virginia")

# albemarle
cc22 <- extract_tables(ccurl22, pages = 56, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc22df <- cc22[[1]]
cc22alb <- cc22df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(6,8,39,40)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville
cc22 <- extract_tables(ccurl22, pages = 64, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc22df <- cc22[[1]]
cc22cvl <- cc22df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(6,8,42,43)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2021 ----
### How well does this translate to 2021?
# row numbers aren't consistent (depends on presence or absence of some kinds of cases)
# ccurl21 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2021/ccms_1011_dec.pdf"
ccurl21 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2021/ccms_1011_dec.pdf"
# define area to read
# pdf21area <- locate_areas(ccurl21, 1) # selected manually
pdfarea <- list(setNames(c(310, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# virginia
cc21 <- extract_tables(ccurl21, pages = 1, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc21df <- cc21[[1]]
cc21va <- cc21df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(9,11,45,46)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Virginia")

# albemarle
cc21 <- extract_tables(ccurl21, pages = 56, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc21df <- cc21[[1]]
cc21alb <- cc21df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(6,8,39,40)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville (had to change slice/rows)
cc21 <- extract_tables(ccurl21, pages = 64, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc21df <- cc21[[1]]
cc21cvl <- cc21df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(9,11,45,46)) %>% # changed
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2020 ----
# December 2020 file is identical to December 2021 file
# ccurl20 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2020/ccms_1011_dec.pdf"
ccurl20 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2020/ccms_1011_dec.pdf"
# define area to read
# pdf20area <- locate_areas(ccurl20, 1) # selected manually
pdfarea <- list(setNames(c(310, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# virginia
cc20 <- extract_tables(ccurl20, pages = 1, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc20df <- cc20[[1]]
cc20va <- cc20df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(9,11,45,46)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Virginia")

# albemarle
cc20 <- extract_tables(ccurl20, pages = 56, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc20df <- cc20[[1]]
cc20alb <- cc20df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(6,8,39,40)) %>% 
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
cc20 <- extract_tables(ccurl20, pages = 64, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc20df <- cc20[[1]]
cc20cvl <- cc20df %>% 
  select(Case.Categories, Total) %>% 
  slice(c(9,11,45,46)) %>% # changed
  mutate(year = Case.Categories,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2019 ----
# different format (different rows, column names, page numbers)
# no statewide summary page - not sure how to get VA
# ccurl19 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2019/ccms_1011_dec.pdf"
ccurl19 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2019/ccms_1011_dec.pdf"
# define area to read
# pdf19area <- locate_areas(ccurl19, 1) # selected manually
pdfarea <- list(setNames(c(310, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc19 <- extract_tables(ccurl19, pages = 55, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc19df <- cc19[[1]]
cc19alb <- cc19df %>% 
  select(X.1, Total) %>% 
  slice(c(5,7,38,39)) %>% 
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
cc19 <- extract_tables(ccurl19, pages = 63, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc19df <- cc19[[1]]
cc19cvl <- cc19df %>% 
  select(X.1, Total) %>% 
  slice(c(5,7,33,34)) %>% # changed
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2018 ----
# no statewide summary page - not sure how to get VA
# ccurl18 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2018/ccms_1011_dec.pdf"
ccurl18 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2018/ccms_1011_dec.pdf"
# define area to read
# pdf18area <- locate_areas(ccurl18, 1) # selected manually
pdfarea <- list(setNames(c(305, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc18 <- extract_tables(ccurl18, pages = 55, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc18df <- cc18[[1]]
cc18alb <- cc18df %>% 
  select(X.1, Total) %>% 
  slice(c(5,7,38,39)) %>% 
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
# table begins higher
pdfarea <- list(setNames(c(290, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))
cc18 <- extract_tables(ccurl18, pages = 63, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc18df <- cc18[[1]]
cc18cvl <- cc18df %>% 
  select(X.1, Total) %>% 
  slice(c(8,10,36,37)) %>% # changed
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2017 ----
# no statewide summary page - not sure how to get VA
# new pages, new rows
# ccurl17 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2017/ccms_1011_dec.pdf"
ccurl17 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2017/ccms_1011_dec.pdf"
# define area to read
# pdf17area <- locate_areas(ccurl17, 1) # selected manually
pdfarea <- list(setNames(c(305, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc17 <- extract_tables(ccurl17, pages = 57, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc17df <- cc17[[1]]
cc17alb <- cc17df %>% 
  select(X, Total) %>% 
  slice(c(4,5,22,23)) %>% 
  mutate(year = X,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(Total, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
# table begins higher
pdfarea <- list(setNames(c(280, 5, 750, 610), 
                         c("top", "left", "bottom", "right")))
cc17 <- extract_tables(ccurl17, pages = 65, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc17df <- cc17[[1]]
cc17cvl <- cc17df %>% 
  select(X, Total) %>% 
  slice(c(2,3,16,17)) %>% # changed
  mutate(year = X,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2016 ----
# no statewide summary page - not sure how to get VA
# new pages, new rows, new area
# ccurl16 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2016/ccms_1011_dec.pdf"
ccurl16 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2016/ccms_1011_dec.pdf"
# define area to read
# pdf16area <- locate_areas(ccurl16, 54) # selected manually
pdfarea <- list(setNames(c(190, 18, 580, 775), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc16 <- extract_tables(ccurl16, pages = 54, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc16df <- cc16[[1]]
cc16alb <- cc16df %>% 
  select(X.1, X.16) %>% 
  slice(c(2,3,21,22)) %>% 
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(X.16, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
cc16 <- extract_tables(ccurl16, pages = 62, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc16df <- cc16[[1]]
cc16cvl <- cc16df %>% 
  select(X.1, X.16) %>% 
  slice(c(2,3,17,18)) %>% # changed
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2015 ----
# no statewide summary page - not sure how to get VA
# new pages, new area, new rows
# ccurl15 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2015/ccms_1011_dec.pdf"
ccurl15 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2015/ccms_1011_dec.pdf"
# define area to read
# pdf15area <- locate_areas(ccurl15, 55) # selected manually
pdfarea <- list(setNames(c(190, 18, 595, 780), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc15 <- extract_tables(ccurl15, pages = 55, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc15df <- cc15[[1]]
cc15alb <- cc15df %>% 
  select(X.1, X.16) %>% 
  slice(c(3,4,19,20)) %>% 
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(X.16, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
cc15 <- extract_tables(ccurl15, pages = 63, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc15df <- cc15[[1]]
cc15cvl <- cc15df %>% 
  select(X.1, X.16) %>% 
  slice(c(3,4,19,20)) %>% # changed
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2014 ----
# no statewide summary page - not sure how to get VA
# new pages, new area, new rows
# ccurl14 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2014/ccms_1011_dec.pdf"
ccurl14 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/ccmsmonthly/2014/ccms_1011_dec.pdf"
# define area to read
# pdf14area <- locate_areas(ccurl14, 2) # selected manually
pdfarea <- list(setNames(c(190, 3, 600, 780), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc14 <- extract_tables(ccurl14, pages = 2, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc14df <- cc14[[1]]
cc14alb <- cc14df %>% 
  select(X.1, X.16) %>% 
  slice(c(4,5,34,35)) %>% 
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = str_remove(X.16, ","),
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Albemarle")

# charlottesville 
cc14 <- extract_tables(ccurl14, pages = 96, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc14df <- cc14[[1]]
cc14cvl <- cc14df %>% 
  select(X.1, X.16) %>% 
  slice(c(3,4,28,29)) %>% # changed
  mutate(year = X.1,
         year = str_extract(year, "[0-9]+"),
         year = str_trim(year),
         year = as.numeric(year),
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")


# 2000-2013 in "Other Report", CBR1- Annual Caseload Report (Through 2013)

## 2013----
ccurl13 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2013.pdf"
# define area to read
# pdf13area <- locate_areas(ccurl13, 139) # selected manually
pdfarea <- list(setNames(c(145, 25, 400, 525), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc13 <- extract_tables(ccurl13, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc13df <- cc13[[1]]
cc13alb <- cc13df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0.1) %>% 
  mutate(year = 2013,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc13 <- extract_tables(ccurl13, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc13df <- cc13[[1]]
cc13cvl <- cc13df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0) %>% 
  mutate(year = 2013,
         locality = "Charlottesville",
         fips = "540") 

# Virginia 
# pdf13area <- locate_areas(ccurl13, 303) # selected manually
pdfarea <- list(setNames(c(144, 23, 400, 325), 
                         c("top", "left", "bottom", "right")))
cc13 <- extract_tables(ccurl13, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc13df <- cc13[[1]]
cc13va <- cc13df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X51) %>% 
  mutate(year = 2013,
         locality = "Virginia",
         fips = "51") 

## 2012----
ccurl12 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2012.pdf"
# define area to read
# pdf12area <- locate_areas(ccurl12, 139) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc12 <- extract_tables(ccurl12, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc12df <- cc12[[1]]
cc12alb <- cc12df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2012,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc12 <- extract_tables(ccurl12, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc12df <- cc12[[1]]
cc12cvl <- cc12df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2012,
         locality = "Charlottesville",
         fips = "540") 

# Virginia 
cc12 <- extract_tables(ccurl12, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc12df <- cc12[[1]]
cc12va <- cc12df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2012,
         locality = "Virginia",
         fips = "51") 

## 2011----
ccurl11 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2011.pdf"
# define area to read
# pdf11area <- locate_areas(ccurl11, 139) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc11 <- extract_tables(ccurl11, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc11df <- cc11[[1]]
cc11alb <- cc11df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2011,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc11 <- extract_tables(ccurl11, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc11df <- cc11[[1]]
cc11cvl <- cc11df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2011,
         locality = "Charlottesville",
         fips = "540") 

# Virginia 
cc11 <- extract_tables(ccurl11, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc11df <- cc11[[1]]
cc11va <- cc11df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2011,
         locality = "Virginia",
         fips = "51") 

## 2010----
ccurl10 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2010.pdf"
# define area to read
# pdf10area <- locate_areas(ccurl10, 139) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc10 <- extract_tables(ccurl10, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc10df <- cc10[[1]]
cc10alb <- cc10df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2010,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc10 <- extract_tables(ccurl10, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc10df <- cc10[[1]]
cc10cvl <- cc10df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2010,
         locality = "Charlottesville",
         fips = "540") 

# Virginia 
cc10 <- extract_tables(ccurl10, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc10df <- cc10[[1]]
cc10va <- cc10df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2010,
         locality = "Virginia",
         fips = "51") 

## 2009----
ccurl09 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2009.pdf"
# define area to read
# pdf09area <- locate_areas(ccurl10, 139) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc09 <- extract_tables(ccurl09, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc09df <- cc09[[1]]
cc09alb <- cc09df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2009,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc09 <- extract_tables(ccurl09, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc09df <- cc09[[1]]
cc09cvl <- cc09df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2009,
         locality = "Charlottesville",
         fips = "540") 

# virginia 
cc09 <- extract_tables(ccurl09, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc09df <- cc09[[1]]
cc09va <- cc09df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2009,
         locality = "Virginia",
         fips = "51")

## 2008----
ccurl08 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2008.pdf"
# define area to read
# pdf09area <- locate_areas(ccurl10, 139) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc08 <- extract_tables(ccurl08, pages = 139, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc08df <- cc08[[1]]
cc08alb <- cc08df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2008,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc08 <- extract_tables(ccurl08, pages = 155, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc08df <- cc08[[1]]
cc08cvl <- cc08df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2008,
         locality = "Charlottesville",
         fips = "540") 

# charlottesville 
cc08 <- extract_tables(ccurl08, pages = 303, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc08df <- cc08[[1]]
cc08va <- cc08df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2008,
         locality = "Virginia",
         fips = "51") 

## 2007----
ccurl07 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2007.pdf"
# define area to read
# pdf07area <- locate_areas(ccurl07, 141) # selected manually
pdfarea <- list(setNames(c(105, 20, 350, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc07 <- extract_tables(ccurl07, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc07df <- cc07[[1]]
cc07alb <- cc07df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2007,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc07 <- extract_tables(ccurl07, pages = 157, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc07df <- cc07[[1]]
cc07cvl <- cc07df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2007,
         locality = "Charlottesville",
         fips = "540") 

# virginia 
cc07 <- extract_tables(ccurl07, pages = 307, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc07df <- cc07[[1]]
cc07va <- cc07df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2007,
         locality = "Virginia",
         fips = "51") 

## 2006----
ccurl06 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2006_2005.pdf"
# define area to read
# pdf06area <- locate_areas(ccurl06, 141) # selected manually
pdfarea <- list(setNames(c(110, 20, 365, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc06 <- extract_tables(ccurl06, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc06df <- cc06[[1]]
cc06alb <- cc06df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2006,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc06 <- extract_tables(ccurl06, pages = 157, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc06df <- cc06[[1]]
cc06cvl <- cc06df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2006,
         locality = "Charlottesville",
         fips = "540") 

# charlottesville 
cc06 <- extract_tables(ccurl06, pages = 307, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc06df <- cc06[[1]]
cc06va <- cc06df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2006,
         locality = "Virginia",
         fips = "51") 

## 2005----
ccurl05 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2005_2004.pdf"
# define area to read
# pdf05area <- locate_areas(ccurl05, 141) # selected manually
pdfarea <- list(setNames(c(109, 35, 285, 410), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc05 <- extract_tables(ccurl05, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc05df <- cc05[[1]]
# not parsing usefully
cc05alb <- data.frame(divorces = 193, year = 2005, locality = "Albemarle", fips = "003")
cc05cvl <- data.frame(divorces = 82, year = 2005, locality = "Charlottesville", fips = "540")
cc05va <- data.frame(divorces = 34231, year = 2005, locality = "Virginia", fips = "51")

## 2004----
ccurl04 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2004_2003.pdf"
# define area to read
# pdf05area <- locate_areas(ccurl05, 141) # selected manually
pdfarea <- list(setNames(c(109, 35, 285, 410), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc04 <- extract_tables(ccurl04, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc04df <- cc04[[1]]
cc04alb <- cc04df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2004,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc04 <- extract_tables(ccurl04, pages = 157, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc04df <- cc04[[1]]
cc04cvl <- cc04df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2004,
         locality = "Charlottesville",
         fips = "540") 

# virginia 
cc04 <- extract_tables(ccurl04, pages = 307, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc04df <- cc04[[1]]
cc04va <- cc04df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X) %>% 
  mutate(year = 2004,
         locality = "Virginia",
         fips = "51") 

## 2003----
ccurl03 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2003_2002.pdf"
# define area to read
# pdf03area <- locate_areas(ccurl03, 141) # selected manually
pdfarea <- list(setNames(c(144, 20, 375, 340), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc03 <- extract_tables(ccurl03, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc03df <- cc03[[1]]
cc03alb <- cc03df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0.1) %>% 
  mutate(year = 2003,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc03 <- extract_tables(ccurl03, pages = 157, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc03df <- cc03[[1]]
cc03cvl <- cc03df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0.1) %>% 
  mutate(year = 2003,
         locality = "Charlottesville",
         fips = "540") 

# virginia 
cc03 <- extract_tables(ccurl03, pages = 307, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc03df <- cc03[[1]]
cc03va <- cc03df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X8) %>% 
  mutate(year = 2003,
         locality = "Virginia",
         fips = "51") 

## 2002----
# 2002 link gives server error message; try second part of 2003
ccurl03 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2003_2002.pdf"
# define area to read
# pdf03area <- locate_areas(ccurl03, 141) # selected manually
pdfarea <- list(setNames(c(140, 340, 375, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc02 <- extract_tables(ccurl03, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc02df <- cc02[[1]]
# nope... create manually
cc02alb <- data.frame(divorces = 205, year = 2002, locality = "Albemarle", fips = "003")
cc02cvl <- data.frame(divorces = 100, year = 2002, locality = "Charlottesville", fips = "540")
cc02va <- data.frame(divorces = 36030, year = 2002, locality = "Virginia", fips = "51")

## 2001----
ccurl01 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2000_2001.pdf"
# define area to read
# pdf01area <- locate_areas(ccurl01, 141) # selected manually
pdfarea <- list(setNames(c(120, 20, 355, 340), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc01 <- extract_tables(ccurl01, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc01df <- cc01[[1]]
cc01alb <- cc01df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0.1) %>% 
  mutate(year = 2001,
         locality = "Albemarle",
         fips = "003") 

# charlottesville 
cc01 <- extract_tables(ccurl01, pages = 157, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc01df <- cc01[[1]]
cc01cvl <- cc01df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X0.1) %>% 
  mutate(year = 2001,
         locality = "Charlottesville",
         fips = "540") 

# virginia 
cc01 <- extract_tables(ccurl01, pages = 307, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc01df <- cc01[[1]]
cc01va <- cc01df %>% 
  filter(str_detect(.[[1]], "TOTAL CASES CONCLUDED")) %>% 
  select(divorces = X21) %>% 
  mutate(year = 2001,
         locality = "Virginia",
         fips = "51") 

## 2000----
# no link that begins with 2000; try 2001 again
ccurl01 <- "https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/stats/circuit/cr01annual/cr01_2000_2001.pdf"
# define area to read
# pdf01area <- locate_areas(ccurl01, 141) # selected manually
pdfarea <- list(setNames(c(116, 335, 355, 540), 
                         c("top", "left", "bottom", "right")))

# albemarle
cc00 <- extract_tables(ccurl01, pages = 141, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

cc00df <- cc00[[1]]
# nope... create manually
cc00alb <- data.frame(divorces = 239, year = 2000, locality = "Albemarle", fips = "003")
cc00cvl <- data.frame(divorces = 155, year = 2000, locality = "Charlottesville", fips = "540")
cc00va <- data.frame(divorces = 35475, year = 2000, locality = "Virginia", fips = "51")


# Bind data frames together ----
ccalb <- bind_rows(mget(ls(pattern = "alb")))
ccalb <- ccalb %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n())

cccvl <- bind_rows(mget(ls(pattern = "cvl")))
cccvl <- cccvl %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n())

ccva <- bind_rows(mget(ls(pattern = "va")))
ccva <- ccva %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n())

divorces <- bind_rows(ccva, ccalb, cccvl) 
divorces <- divorces %>% 
  group_by(locality) %>% 
  fill(fips)

# Pull in population data ----
## and construct rates
# # using cdc pop data
# pop <- read_csv("data/pop_data_cdc.csv")
# pop <- pop %>% 
#   mutate(locality = case_when(
#     fips == 51003 ~ "Albemarle",
#     fips == 51540 ~ "Charlottesville",
#     fips == 51 ~ "Virginia"
#     ),
#     pop_cdc = pop_tot)
# 
# divorces_pop <- divorces %>% 
#   left_join(pop %>% 
#               select(fips, year, pop_cdc, locality))
# 
# divorces_pop <- divorces_pop %>% 
#   mutate(divorce_rate1 = (divorces/pop_cdc)*1000)

# using wcc pop data
pop <- read_csv("data/pop_data_wcc.csv")
pop <- pop %>% 
  mutate(locality2 = locality,
         locality = case_when(
           locality == "Charlottesville City" ~ "Charlottesville",
           locality == "Albemarle County" ~ "Albemarle",
           locality == "Virginia" ~ "Virginia"
         ))

divorces_pop <- divorces %>% 
  left_join(pop)

divorces_pop <- divorces_pop %>% 
  mutate(divorce_rate = (divorces/pop_wcc)*1000)

# have a peek
ggplot(divorces_pop, aes(x = year, y = divorce_rate, color = locality)) +
  geom_line()


# Save ----
write_csv(divorces_pop, "data/divorce_circuit.csv")
