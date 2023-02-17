# Stepping Stones Data: Children in need of services, Delinquency ----
# Updated 2023-02-14
# Contributor: Michele Claibourn
# Acquire data from Virginia's Judicial System Caseload Statistical Information
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Judicial System, Caseload Statistical Information, "Caseload Statistics of the Juvenile and Domestic Relations Courts." 2018-2022.
# https://www.vacourts.gov/courtadmin/aoc/judpln/csi/home.html


# Libraries ----
library(tidyverse)
library(tabulizer)


# Read in a single pdf report ----
## Virgina ----
# 2022
jdrurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2022/jcms1011_dec.pdf"
# define area to read
# pdf22area <- locate_areas(jdrurl22, 2) # selected manually
pdfarea <- list(setNames(c(215, 5, 380, 605), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 2, area = pdfarea, 
                       guess = FALSE, output = "data.frame")

jdr22df <- jdr22[[1]]
jdr22va <- jdr22df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2022, locality = "Virginia",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2021
jdrurl21 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2021/jcms1011_dec.pdf")
# define area to read
# pdf21area <- locate_areas(jdrurl21, 2) # selected manually
pdfarea <- list(setNames(c(180, 6, 355, 610), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21df <- jdr21[[1]]
jdr21va <- jdr21df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2021, locality = "Virginia",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2020
jdrurl20 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2020/jcms1011_dec.pdf")
# define area to read
# pdf20area <- locate_areas(jdrurl20, 2) # selected manually
pdfarea <- list(setNames(c(199, 7, 366, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20df <- jdr20[[1]]
jdr20va <- jdr20df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2020, locality = "Virginia",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2019
jdrurl19 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2019/jcms1011_dec.pdf")
# define area to read
# pdf19area <- locate_areas(jdrurl19, 2) # selected manually
pdfarea <- list(setNames(c(176, 7, 332, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 2, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19df <- jdr19[[1]]
jdr19va <- jdr19df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2019, locality = "Virginia",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2018
jdrurl18 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2018/jcms1011_dec.pdf")
# define area to read
# pdf18area <- locate_areas(jdrurl18, 1) # selected manually
pdfarea <- list(setNames(c(148, 7, 289, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 1, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18va <- jdr18df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2018, locality = "Virginia",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 


## Albemarle ----
# 2022
jdrurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2022/jcms1011_dec.pdf"
# define area to read
# pdf22area <- locate_areas(jdrurl22, 108) # selected manually
pdfarea <- list(setNames(c(55, 7, 210, 610), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 108, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22df <- jdr22[[1]]
jdr22alb <- jdr22df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2022, locality = "Albemarle",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2021
jdrurl21 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2021/jcms1011_dec.pdf")
# define area to read
# pdf21area <- locate_areas(jdrurl21, 108) # selected manually
pdfarea <- list(setNames(c(55, 7, 210, 610), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 108, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21df <- jdr21[[1]]
jdr21alb <- jdr21df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2021, locality = "Albemarle",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2020
jdrurl20 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2020/jcms1011_dec.pdf"
# define area to read
# pdf20area <- locate_areas(jdrurl20, 107) # selected manually
# top half of table
pdfarea <- list(setNames(c(687, 7, 760, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 106, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20dfa <- jdr20[[1]]

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 132, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 107, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20dfb <- jdr20[[1]]
names(jdr20dfb) <- names(jdr20dfa)
jdr20df <- bind_rows(jdr20dfa, jdr20dfb)

jdr20alb <- jdr20df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2020, locality = "Albemarle",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2019
jdrurl19 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2019/jcms1011_dec.pdf")
# define area to read
# pdf19area <- locate_areas(jdrurl19, 108) # selected manually
# top half of table
pdfarea <- list(setNames(c(707, 7, 760, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 107, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19dfa <- jdr19[[1]]
jdr19dfa <- jdr19dfa %>% 
  mutate(Summary = as.character(Summary))

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 160, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 108, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19dfb <- jdr19[[1]]
names(jdr19dfb) <- names(jdr19dfa)
jdr19df <- bind_rows(jdr19dfa, jdr19dfb)

jdr19alb <- jdr19df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2019, locality = "Albemarle",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 

# 2018
jdrurl18 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2018/jcms1011_dec.pdf")
# define area to read
# pdf18area <- locate_areas(jdrurl18, 98) # selected manually
pdfarea <- list(setNames(c(148, 7, 289, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 98, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18alb <- jdr18df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2018, locality = "Albemarle",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 


## Charlottesville ----
# 2022
jdrurl22 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2022/jcms1011_dec.pdf"
# define area to read
# pdf22area <- locate_areas(jdrurl22, 110) # selected manually
# top half of table
pdfarea <- list(setNames(c(685, 7, 764, 610), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22dfa <- jdr22[[1]]

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 134, 610), 
                         c("top", "left", "bottom", "right")))

jdr22 <- extract_tables(jdrurl22, pages = 110, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr22dfb <- jdr22[[1]]
names(jdr22dfb) <- names(jdr22dfa)
jdr22df <- bind_rows(jdr22dfa, jdr22dfb)

jdr22cvl <- jdr22df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2022, locality = "Charlottesville",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count)

# 2021
jdrurl21 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2021/jcms1011_dec.pdf")
# define area to read
# pdf21area <- locate_areas(jdrurl21, 110) # selected manually
# top half of table
pdfarea <- list(setNames(c(685, 7, 764, 610), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21dfa <- jdr21[[1]]

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 135, 610), 
                         c("top", "left", "bottom", "right")))

jdr21 <- extract_tables(jdrurl21, pages = 110, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr21dfb <- jdr21[[1]]
names(jdr21dfb) <- names(jdr21dfa)
jdr21df <- bind_rows(jdr21dfa, jdr21dfb)

jdr21cvl <- jdr21df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2021, locality = "Charlottesville",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count)

# 2020
jdrurl20 <- "https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2020/jcms1011_dec.pdf"
# define area to read
# pdf20area <- locate_areas(jdrurl20, 109) # selected manually
# top half of table
pdfarea <- list(setNames(c(696, 7, 762, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 108, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20dfa <- jdr20[[1]]

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 148, 610), 
                         c("top", "left", "bottom", "right")))

jdr20 <- extract_tables(jdrurl20, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr20dfb <- jdr20[[1]]
names(jdr20dfb) <- names(jdr20dfa)
jdr20df <- bind_rows(jdr20dfa, jdr20dfb)

jdr20cvl <- jdr20df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2020, locality = "Charlottesville",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count)

# 2019
jdrurl19 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2021/jcms1011_dec.pdf")
# define area to read
# pdf19area <- locate_areas(jdrurl19, 110) # selected manually
# top half of table
pdfarea <- list(setNames(c(690, 7, 762, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 109, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19dfa <- jdr19[[1]]

# bottom half of table
pdfarea <- list(setNames(c(25, 7, 135, 610), 
                         c("top", "left", "bottom", "right")))

jdr19 <- extract_tables(jdrurl19, pages = 110, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr19dfb <- jdr19[[1]]
names(jdr19dfb) <- names(jdr19dfa)
jdr19df <- bind_rows(jdr19dfa, jdr19dfb)

jdr19cvl <- jdr19df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2019, locality = "Charlottesville",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count)

# 2018
jdrurl18 <- c("https://www.vacourts.gov/courtadmin/aoc/judpln/csi/stats/jdr/cms/2018/jcms1011_dec.pdf")
# define area to read
# pdf18area <- locate_areas(jdrurl18, 100) # selected manually
pdfarea <- list(setNames(c(148, 7, 289, 610), 
                         c("top", "left", "bottom", "right")))

jdr18 <- extract_tables(jdrurl18, pages = 100, area = pdfarea, 
                        guess = FALSE, output = "data.frame")

jdr18df <- jdr18[[1]]
jdr18cvl <- jdr18df %>% 
  filter(Category %in% c("Child in Need of Services/Supervision", "Delinquency")) %>% 
  mutate(year = 2018, locality = "Charlottesville",
         count = str_remove(Summary, ","),
         count = as.numeric(count)) %>% 
  select(year, locality, type = Category, count) 


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

# divide into chins/delinq
chins <- jdr %>% 
  filter(type == "Child in Need of Services/Supervision")
delinq <- jdr %>% 
  filter(type == "Delinquency")

# have a peek
ggplot(chins, aes(x = year, y = rate, color = locality)) +
  geom_line()

ggplot(delinq, aes(x = year, y = rate, color = locality)) +
  geom_line()


# Save data ----
write_csv(chins, "data/child_need_services.csv")
write_csv(delinq, "data/juvenile_delinquency.cscv")
