# Stepping Stones Data: Children in need of services/supervision ----
# Updated 2023-02-14
# Contributor: Michele Claibourn
# Acquire data from Virginia's Judicial System Caseload Statistical Information
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Judicial System, Caseload Statistical Information, "Caseload Statistics of the Juvenile and Domestic Relations Courts." 2018-2022.
# https://www.vacourts.gov/courtadmin/aoc/judpln/csi/home.html

# Children in need of services/supervision: truancy + child in need of services
# 10.11 Monthly Case Dispositions YTD by Case Category, 2018-2022
# Caseload Reports, 2010-2018

# Libraries ----
library(tidyverse)
library(tabulizer)


# 2022 report ----
jdrurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2022/jcms1011_dec.pdf"

## Virginia ----
# pdf22area <- locate_areas(jdrurl22, 1) # selected manually
pdfarea <- list(setNames(c(165, 9, 765, 605), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22df <- jdr22[[1]]
jdr22va <- jdr22df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2022, locality = "Virginia")

### Albemarle ----
# pdf22area <- locate_areas(jdrurl22, 107) # selected manually
pdfarea <- list(setNames(c(165, 9, 677, 605), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 107, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22df <- jdr22[[1]]
jdr22alb <- jdr22df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2022, locality = "Albemarle")

### Charlottesville ----
# pdf22area <- locate_areas(jdrurl22, 109) # selected manually
pdfarea <- list(setNames(c(170, 8, 625, 605), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22df <- jdr22[[1]]
jdr22cvl <- jdr22df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2022, locality = "Charlottesville")


# 2021 ----
jdrurl21 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2021/jcms1011_dec.pdf")

## Virgina ----
# pdf21area <- locate_areas(jdrurl21, 1) # selected manually
pdfarea <- list(setNames(c(170, 9, 760, 610), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21df <- jdr21[[1]]
jdr21va <- jdr21df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2021, locality = "Virginia")

### Albemarle ----
# pdf21area <- locate_areas(jdrurl21, 107) # selected manually
pdfarea <- list(setNames(c(165, 9, 665, 605), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 107, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21df <- jdr21[[1]]
jdr21alb <- jdr21df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2021, locality = "Albemarle")

### Charlottesville ----
# pdf21area <- locate_areas(jdrurl21, 109) # selected manually
pdfarea <- list(setNames(c(170, 9, 625, 605), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21df <- jdr21[[1]]
jdr21cvl <- jdr21df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2021, locality = "Charlottesville")


# 2020 ----
jdrurl20 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2020/jcms1011_dec.pdf")

## Virginia ----
# pdf20area <- locate_areas(jdrurl20, 1) # selected manually
pdfarea <- list(setNames(c(170, 8, 765, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20df <- jdr20[[1]]
jdr20va <- jdr20df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2020, locality = "Virginia")

## Albemarle ----
# pdf20area <- locate_areas(jdrurl20, 106) # selected manually
pdfarea <- list(setNames(c(165, 9, 625, 605), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 106, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20df <- jdr20[[1]]
jdr20alb <- jdr20df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2020, locality = "Albemarle")

## Charlottesville ----
# pdf20area <- locate_areas(jdrurl20, 108) # selected manually
pdfarea <- list(setNames(c(170, 9, 630, 605), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 108, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20df <- jdr20[[1]]
jdr20cvl <- jdr20df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2020, locality = "Charlottesville")


# 2019 ----
jdrurl19 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2019/jcms1011_dec.pdf")

## Virginia ----
# pdf19area <- locate_areas(jdrurl19, 1) # selected manually
pdfarea <- list(setNames(c(169, 7, 765, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19df <- jdr19[[1]]
jdr19va <- jdr19df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2019, locality = "Virginia")

## Albemarle ----
# pdf19area <- locate_areas(jdrurl19, 107) # selected manually
pdfarea <- list(setNames(c(169, 7, 640, 605), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 107, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19df <- jdr19[[1]]
jdr19alb <- jdr19df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2019, locality = "Albemarle")

## Charlottesville ----
# pdf19area <- locate_areas(jdrurl19, 109) # selected manually
pdfarea <- list(setNames(c(170, 6, 600, 605), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19df <- jdr19[[1]]
jdr19cvl <- jdr19df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description.Code, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2019, locality = "Charlottesville")


# 2018 ----
jdrurl18 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2018/jcms1011_dec.pdf")

## Virginia ----
# pdf18area <- locate_areas(jdrurl18, 1) # selected manually
pdfarea <- list(setNames(c(344, 9, 735, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18va <- jdr18df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2018, locality = "Virginia")

## Albemarle ----
# pdf18area <- locate_areas(jdrurl18, 98) # selected manually
pdfarea <- list(setNames(c(342, 10, 627, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 98, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18alb <- jdr18df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2018, locality = "Albemarle")

## Charlottesville ----
# pdf18area <- locate_areas(jdrurl18, 100) # selected manually
pdfarea <- list(setNames(c(342, 10, 627, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 100, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18cvl <- jdr18df %>% 
  mutate(Division = ifelse(Division == "", NA_character_, Division)) %>% 
  fill(Division, .direction = "down") %>% 
  filter(Division == "Juvenile", str_detect(Case.Type.Description, "Truancy|Child In Need")) %>% 
  mutate(count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2018, locality = "Charlottesville")


# 2017 ----
jdrurl17 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2017.pdf"

## Albemarle ----
# pdf17area <- locate_areas(jdrurl17, 75) # selected manually
pdfarea <- list(setNames(c(35, 5, 475, 607), 
                         c("top", "left", "bottom", "right")))

jdr17 <- extract_tables(jdrurl17, pages = 75, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr17df <- jdr17[[1]]
jdr17alb <- jdr17df[which(jdr17df$X == "Juvenile"):nrow(jdr17df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2017, locality = "Albemarle")

## Charlottesville ----
# pdf17area <- locate_areas(jdrurl17, 76) # selected manually
pdfarea <- list(setNames(c(35, 5, 465, 600), 
                         c("top", "left", "bottom", "right")))

jdr17 <- extract_tables(jdrurl17, pages = 76, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr17df <- jdr17[[1]]
jdr17cvl <- jdr17df[which(jdr17df$X == "Juvenile"):nrow(jdr17df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2017, locality = "Charlottesville")


# 2016 ----
jdrurl16 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2016.pdf"

## Albemarle ----
# pdf16area <- locate_areas(jdrurl16, 2) # selected manually
pdfarea <- list(setNames(c(57, 10, 525, 605), 
                         c("top", "left", "bottom", "right")))

jdr16 <- extract_tables(jdrurl16, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr16df <- jdr16[[1]]
jdr16alb <- jdr16df[which(jdr16df$X == "Juvenile"):nrow(jdr16df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2016, locality = "Albemarle")

## Charlottesville ----
# pdf16area <- locate_areas(jdrurl16, 24) # selected manually
pdfarea <- list(setNames(c(57, 10, 520, 610), 
                         c("top", "left", "bottom", "right")))

jdr16 <- extract_tables(jdrurl16, pages = 24, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr16df <- jdr16[[1]]
jdr16cvl <- jdr16df[which(jdr16df$X == "Juvenile"):nrow(jdr16df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2016, locality = "Charlottesville")


# 2015 ----
jdrurl15 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2015.pdf"

## Albemarle ----
# pdf15area <- locate_areas(jdrurl15, 2) # selected manually
pdfarea <- list(setNames(c(58, 10, 525, 610), 
                         c("top", "left", "bottom", "right")))

jdr15 <- extract_tables(jdrurl15, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr15df <- jdr15[[1]]
jdr15alb <- jdr15df[which(jdr15df$X == "Juvenile"):nrow(jdr15df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2015, locality = "Albemarle")

## Charlottesville ----
# pdf15area <- locate_areas(jdrurl15, 24) # selected manually
pdfarea <- list(setNames(c(58, 10, 510, 610), 
                         c("top", "left", "bottom", "right")))

jdr15 <- extract_tables(jdrurl15, pages = 24, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr15df <- jdr15[[1]]
jdr15cvl <- jdr15df[which(jdr15df$X == "Juvenile"):nrow(jdr15df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2015, locality = "Charlottesville")


# 2014 ----
jdrurl14 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2014.pdf"

## Albemarle ----
# pdf14area <- locate_areas(jdrurl14, 3) # selected manually
pdfarea <- list(setNames(c(75, 12, 625, 600), 
                         c("top", "left", "bottom", "right")))

jdr14 <- extract_tables(jdrurl14, pages = 3, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr14df <- jdr14[[1]]
jdr14alb <- jdr14df[which(jdr14df$X == "Juvenile"):nrow(jdr14df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2014, locality = "Albemarle")

## Charlottesville ----
# pdf14area <- locate_areas(jdrurl14, 25) # selected manually
pdfarea <- list(setNames(c(75, 12, 545, 600), 
                         c("top", "left", "bottom", "right")))

jdr14 <- extract_tables(jdrurl14, pages = 25, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr14df <- jdr14[[1]]
jdr14cvl <- jdr14df[which(jdr14df$X == "Juvenile"):nrow(jdr14df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2014, locality = "Charlottesville")


# 2013 ----
jdrurl13 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2013.pdf"

## Albemarle ----
# pdf13area <- locate_areas(jdrurl13, 3) # selected manually
pdfarea <- list(setNames(c(74, 12, 565, 600), 
                         c("top", "left", "bottom", "right")))

jdr13 <- extract_tables(jdrurl13, pages = 3, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr13df <- jdr13[[1]]
jdr13alb <- jdr13df[which(jdr13df$X == "Juvenile"):nrow(jdr13df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2013, locality = "Albemarle")

## Charlottesville ----
# pdf13area <- locate_areas(jdrurl13, 25) # selected manually
pdfarea <- list(setNames(c(73, 12, 630, 600), 
                         c("top", "left", "bottom", "right")))

jdr13 <- extract_tables(jdrurl13, pages = 25, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr13df <- jdr13[[1]]
jdr13cvl <- jdr13df[which(jdr13df$X == "Juvenile"):nrow(jdr13df),] %>% 
  filter(str_detect(X, "Truancy|Child in Need")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2013, locality = "Charlottesville")


# 2012 ----
jdrurl12 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2012.pdf"

## Albemarle ----
# pdf12area <- locate_areas(jdrurl12, 2) # selected manually
pdfarea <- list(setNames(c(74, 12, 530, 600), 
                         c("top", "left", "bottom", "right")))

jdr12 <- extract_tables(jdrurl12, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr12df <- jdr12[[1]]
jdr12alb <- jdr12df[which(jdr12df$X == "Juvenile"):nrow(jdr12df),] %>% 
  filter(str_detect(X, "Truancy|CHINS")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2012, locality = "Albemarle")

## Charlottesville ----
# pdf12area <- locate_areas(jdrurl12, 24) # selected manually
pdfarea <- list(setNames(c(74, 12, 575, 600), 
                         c("top", "left", "bottom", "right")))

jdr12 <- extract_tables(jdrurl12, pages = 24, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr12df <- jdr12[[1]]
jdr12cvl <- jdr12df[which(jdr12df$X == "Juvenile"):nrow(jdr12df),] %>% 
  filter(str_detect(X, "Truancy|CHINS")) %>% 
  mutate(count = str_remove(Concluded, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2012, locality = "Charlottesville")


# 2011 ----
jdrurl11 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2011.pdf"

## Albemarle ----
# pdf11area <- locate_areas(jdrurl11, 109) # selected manually
pdfarea <- list(setNames(c(86, 15, 550, 600), 
                         c("top", "left", "bottom", "right")))

jdr11 <- extract_tables(jdrurl11, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr11df <- jdr11[[1]]
jdr11alb <- jdr11df[-which(str_detect(jdr11df$X, "DOMESTIC")):-nrow(jdr11df),] %>% 
  filter(str_detect(X, "TRUANCY|CHILD NEED")) %>% 
  mutate(count = str_remove(Final, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2011, locality = "Albemarle")

## Charlottesville ----
# pdf11area <- locate_areas(jdrurl11, 125) # selected manually
pdfarea <- list(setNames(c(86, 15, 530, 600), 
                         c("top", "left", "bottom", "right")))

jdr11 <- extract_tables(jdrurl11, pages = 125, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr11df <- jdr11[[1]]
jdr11cvl <- jdr11df[-which(str_detect(jdr11df$X, "DOMESTIC")):-nrow(jdr11df),] %>% 
  filter(str_detect(X, "TRUANCY|CHILD NEED")) %>% 
  mutate(count = str_remove(Final, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2011, locality = "Charlottesville")


# 2010 ----
jdrurl10 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/ei01_2010.pdf"

## Albemarle ----
# pdf10area <- locate_areas(jdrurl10, 109) # selected manually
pdfarea <- list(setNames(c(95, 15, 515, 600), 
                         c("top", "left", "bottom", "right")))

jdr10 <- extract_tables(jdrurl10, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr10df <- jdr10[[1]]
jdr10alb <- jdr10df[-which(str_detect(jdr10df$X, "DOMESTIC")):-nrow(jdr10df),] %>% 
  filter(str_detect(X, "TRUANCY|CHILD NEED")) %>% 
  mutate(count = str_remove(Final, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2010, locality = "Albemarle")

## Charlottesville ----
# pdf10area <- locate_areas(jdrurl10, 125) # selected manually
pdfarea <- list(setNames(c(95, 15, 515, 600), 
                         c("top", "left", "bottom", "right")))

jdr10 <- extract_tables(jdrurl10, pages = 125, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr10df <- jdr10[[1]]
jdr10cvl <- jdr10df[-which(str_detect(jdr10df$X, "DOMESTIC")):-nrow(jdr10df),] %>% 
  filter(str_detect(X, "TRUANCY|CHILD NEED")) %>% 
  mutate(count = str_remove(Final, ","),
         count = as.numeric(count)) %>% 
  summarize(count = sum(count)) %>% 
  mutate(year = 2010, locality = "Charlottesville")


# Bind rows ----
jdr <- bind_rows(mget(ls(pattern = "va$|alb$|cvl$")))

jdr <- jdr %>% 
  mutate(fips = case_when(
    locality == "Albemarle" ~ "51003",
    locality == "Charlottesville" ~ "51540",
    locality == "Virginia" ~ "51"
  ))

# Read in population data ----
pop <- read_csv("data/pop_data_cdc.csv")

pop <- pop %>% 
  select(fips, year, pop_1017) %>% 
  mutate(fips = as.character(fips))


# Join, create rate, split types ----
jdr <- left_join(jdr, pop)

# fill in pop data for 2021 and generate rate
jdr <- jdr %>% 
  group_by(locality) %>% 
  fill(pop_1017, .direction = "down") %>% 
  ungroup()

# create rate per 1000
jdr <- jdr %>% 
  mutate(rate = (count/pop_1017)*1000)

# have a peek
ggplot(jdr, aes(x = year, y = rate, color = locality)) +
  geom_line()

jdr %>% filter(year >= 2018) %>% 
  ggplot(aes(x = year, y = rate, color = locality)) +
  geom_line()

# Save data ----
write_csv(jdr, "data/child_need_services.csv")
