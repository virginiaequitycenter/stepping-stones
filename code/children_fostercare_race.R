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


# # Downloaded in children_fostercare.R ----
# repeat lines 18-45
# Create vector of urls
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


# # download files ----
# if (!dir.exists(here("datadownloads/dssfc"))) 
#   {dir.create(here("datadownloads/dssfc"))}
# 
# walk2(urls, dest, download.file, method = "curl", extra = "-k")


# read in files ----
# test <- read_excel("datadownloads/dssfc/fc_demo_202209.xlsx", sheet = 1, skip = 3)
#test <- read_excel(dest[1], sheet = 1, skip = 3)

fc_data <- map(dest, ~read_excel(.x, sheet = 1, skip = 3))
names(fc_data) <- 2022:2006 # add year as names for list

fc_all <- bind_rows(fc_data, .id = "year")


# Pull cville, alb, state data ----

fc_race <- fc_all %>% 
  filter(LOCALITY %in% c("Albemarle", "Charlottesville", "STATE")) %>% 
  clean_names() %>% 
  select(year, locality, fips, count_black = black, percent_black,
         count_white = white, percent_white, count_aian = am_indian_alaskan_native,
         percent_aian = percent_am_indian_alaskan_native, count_asian = asian,
         percent_asian, count_nhpi = hawaiian_pacific_islander, 
         percent_nhpi = percent_hawaiian_pacific_isl, count_multi = multi_race,
         percent_multi = percent_multi_race, count_unknown = race_unknown, 
         percent_unknown = percent_unknown_25, count_hispanic = hispanic, percent_hispanic) %>% 
  mutate(year = as.numeric(year),
         locality = ifelse(locality == "STATE", "Virginia", locality),
         fips = case_when(
           fips == 3 ~ "51003",
           fips == 540 ~ "51540",
           is.na(fips) ~ "51"
         ))

# Pivot for visualization ----

fc_race_long <- fc_race %>% 
  pivot_longer(cols = -c("year", "locality", "fips"),
               names_to = c("type", "race"),
               names_sep = "_",
               values_to = "value")

fc_race_long <- fc_race_long %>% 
  pivot_wider(names_from = "type", values_from = "value")

# have a peek
# race
fc_race_long %>% 
  filter(race != "hispanic") %>% 
  ggplot(aes(x = year, y = count, fill = race)) +
  geom_col(position = "fill") +
  facet_wrap(~locality)

# ethnicity
fc_race_long %>% 
  filter(race == "hispanic") %>% 
  ggplot(aes(x = year, y = percent)) +
  geom_col() +
  facet_wrap(~locality)


# Pull in population data, join, compare percent ----
# cdc age, race estimates only have aian/hpi, asian, black, white
# missing multiracial (substantial in this data); fc data starts in 2006
# missing hispanic/ethnicity; hispanic starts in 2010 in fc data
# pull estimates from ACS instead; can only go back to 2009

# read in child race data from ACS when complete
child_pop <- read_csv("data/child_pop_race_ethn.csv")

child_pop <- child_pop %>% 
  mutate(fips = as.character(GEOID),
         race = ifelse(is.na(race) & ethn == "hisp", "hispanic", race))

# join to fc_race_long
fc_race_pop <- fc_race_long %>% 
  left_join(child_pop)

# fill in 2022 pop with 2021 pop values
fc_race_pop <- fc_race_pop %>% 
  group_by(locality, fips, race) %>% 
  fill(c(pop_count, total_count, pop_percent), .direction = "up") %>% 
  ungroup()

# have a peek
# cville
fc_race_pop %>% 
  filter(race %in% c("white", "black", "multi", "asian", "hispanic"),
         locality == "Charlottesville", year > 2008) %>% 
  select(year, locality, fips, race, fc_percent = percent, pop_percent) %>%
  pivot_longer(cols = c(fc_percent, pop_percent), names_to = "type", values_to = "values") %>% 
  ggplot(aes(x = year, y = values)) +
  geom_point(aes(color = type)) +
  geom_line(aes(group = year), color = "grey") +
  facet_wrap(~race) 

# alb
fc_race_pop %>% 
  filter(race %in% c("white", "black", "multi", "asian", "hispanic"),
         locality == "Albemarle", year > 2008) %>% 
  select(year, locality, fips, race, fc_percent = percent, pop_percent) %>%
  pivot_longer(cols = c(fc_percent, pop_percent), names_to = "type", values_to = "values") %>% 
  ggplot(aes(x = year, y = values)) +
  geom_point(aes(color = type)) +
  geom_line(aes(group = year), color = "grey") +
  facet_wrap(~race) 

# save data ----
fc_race_pop <- fc_race_pop %>% 
  filter(year > 2008)
write_csv(fc_race_pop, "data/foster_care_race.csv")
