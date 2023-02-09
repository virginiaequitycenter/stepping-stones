# Stepping Stones Data: Divorces ----
# Updated 2023-02-03
# Contributor: Michele Claibourn
# Acquire data from Virginia's Judicial System Caseload Statistical Information
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Judicial System, Caseload Statistical Information, "Caseload Statistics of the Circuit Courts." 2014-2022.
# https://www.vacourts.gov/courtadmin/aoc/judpln/csi/home.html


# Libraries ----
library(tidyverse)
library(tabulizer)


# Read in a single pdf report ----
## 2022 ----
ccurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2022/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2021 ----
### How well does this translate to 2021?
# row numbers aren't consistent (depends on presence or absence of some kinds of cases)
ccurl21 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2021/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2020 ----
# December 2020 file is identical to December 2021 file
ccurl20 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2020/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2019 ----
# different format (different rows, column names, page numbers)
# no statewide summary page - not sure how to get VA
ccurl19 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2019/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2018 ----
# no statewide summary page - not sure how to get VA
ccurl18 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2018/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2017 ----
# no statewide summary page - not sure how to get VA
# new pages, new rows
ccurl17 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2017/ccms_1011_dec.pdf"
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
         Total = as.numeric(Total)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2016 ----
# no statewide summary page - not sure how to get VA
# new pages, new rows, new area
ccurl16 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2016/ccms_1011_dec.pdf"
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
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2015 ----
# no statewide summary page - not sure how to get VA
# new pages, new area, new rows
ccurl15 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2015/ccms_1011_dec.pdf"
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
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")

## 2014 ----
# no statewide summary page - not sure how to get VA
# new pages, new area, new rows
ccurl14 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/circuit/ccmsmonthly/2014/ccms_1011_dec.pdf"
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
         Total = as.numeric(X.16)) %>% 
  group_by(year) %>% 
  summarize(divorces = sum(Total)) %>% 
  mutate(locality = "Charlottesville")


# Bind data frames together ----
ccalb <- bind_rows(mget(ls(pattern = "alb")))
ccalb <- ccalb %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n = n())
  
cccvl <- bind_rows(mget(ls(pattern = "cvl")))
cccvl <- cccvl %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n = n())

ccva <- bind_rows(mget(ls(pattern = "va")))
ccva <- ccva %>% 
  arrange(year, divorces) %>% 
  group_by(year) %>% 
  slice(n = n())

divorces <- bind_rows(ccva, ccalb, cccvl) %>% 
  mutate(year = as.numeric(year))


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

# consider 2000-2018 vdh data
# https://apps.vdh.virginia.gov/HealthStats/stats.htm
