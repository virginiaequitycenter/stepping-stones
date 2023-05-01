# Stepping Stones Data: Foster care ----
# Updated 2023-02-11
# Contributor: Michele Claibourn
# Acquire data from Virginia DSS on Children in Foster Care
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Social Services, Foster Care Reports, "Children Demographics." 2006-2022.
# https://www.dss.virginia.gov/geninfo/reports/children/fc.cgi


# Libraries ----
library(tidyverse)
library(here)
library(readxl)
library(janitor)


# Create vector of urls ----
## choose september report for each year (manually copy-paste)
## (could pull all reports and average as well)

urls <- c(
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2022/children_demographic/fc_demo_202209.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2021/children_demographic/fc_demo_202109.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2020/children_demographic/fc_demo_202009.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2019/children_demographic/fc_demo_201909.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2018/children_demographic/fc_demo_2018_09.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2017/children_demographic/fc_demo_2017_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2016/children_demographic/fc_demo_2016_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2015/children_demographic/fc_demo_2015_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2014/children_demographic/fc_demo_2014_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2013/children_demographic/fc_demo_2013_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2012/children_demographic/fc_demo_2012_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2011/children_demographic/fc_demo_2011_09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2010/children_demographic/fc_children_demo_2010-09-30.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2009/children_demographic/fc_children_demo_2009-09-30.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2008/children_demographic/fc_children_demographic_2008-09.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2007/children_demographic/09-2007.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2006/children_demographic/09-2006.xls"
)

# create vector of destination file names ----
dest <- paste0("datadownloads/dssfc/", basename(urls))


# download files ----
if (!dir.exists(here("datadownloads/dssfc"))) 
  {dir.create(here("datadownloads/dssfc"))}

walk2(urls, dest, download.file, method = "curl", extra = "-k")


# read in files ----
# test <- read_excel("datadownloads/dssfc/fc_demo_202209.xlsx", sheet = 1, skip = 3)
#test <- read_excel(dest[1], sheet = 1, skip = 3)

fc_data <- map(dest, ~read_excel(.x, sheet = 1, skip = 3))
names(fc_data) <- 2022:2006 # add year as names for list

fc_all <- bind_rows(fc_data, .id = "year")


# Pull cville, alb, state data ----

fc <- fc_all %>% 
  filter(LOCALITY %in% c("Albemarle", "Charlottesville", "STATE")) %>% 
  clean_names() %>% 
  select(year, locality, fips, fc_count = total_children_in_care) %>% 
  mutate(year = as.numeric(year),
         locality = ifelse(locality == "STATE", "Virginia", locality),
         fips = case_when(
           fips == 3 ~ "51003",
           fips == 540 ~ "51540",
           is.na(fips) ~ "51"
         ))


# Pull in population data, join, create percent ----
pop <- read_csv("data/pop_data_cdc.csv") %>% 
  mutate(fips = as.character(fips)) %>% 
  select(fips, year, pop_17under)

# join pop to foster care
fc <- fc %>% 
  left_join(pop)

# fill in 2021 and 2022 pop with 2020 numbers
fc <- fc %>% 
  group_by(locality) %>% 
  fill(pop_17under, .direction = "up") %>% 
  ungroup()

# create rate per 1000
fc <- fc %>% 
  mutate(fc_rate = (fc_count/pop_17under)*1000)

# have a peek
ggplot(fc, aes(x = year, y = fc_rate, color = locality)) +
         geom_line()

# save data ----
write_csv(fc, "data/foster_care.csv")
# fc <- read_csv("data/foster_care.csv")
