# Stepping Stones Data: Child Welfare Reports ----
# Updated 2023-02-11
# Contributor: Michele Claibourn
# Acquire data from Virginia DSS Child Protection and Accountability System
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Social Services, The Virginia Child Protection Accountability System, "Annual Summary." 2009-2020.
# https://cpsaccountability.dss.virginia.gov/index-historical-reports.html


# Libraries ----
library(tidyverse)
library(here)
library(readxl)
library(janitor)


# Create vector of urls ----

urls <- c(
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2020/cps_historical_report_sfy_2020.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2019/cps_historical_report_sfy_2019.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2018/cps_historical_report_sfy_2018.xls",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2017/Referrals_and_Findings_SFY2017.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2016/Referrals_and_Findings_SFY2016.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2015/Referrals_and_Findings_SFY2015.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2014/Referrals_and_Findings_SFY2014.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2013/Referrals_and_Findings_SFY2013.xlsx",
  "http://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2012/Referrals_and_Findings_SFY2012.xlsx",
  "http://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2011/Referrals_and_Findings_SFY2011.xlsx",
  "http://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2010/Referrals_and_Findings_SFY2010.xlsx",
  "http://www.dss.virginia.gov/files/about/reports/children/cps/accountability/2009/Referrals_and_Findings_SFY2009.xlsx"
)

# create vector of destination file names ----
dest <- paste0("datadownloads/dsscps/", basename(urls))


# download files ----
if (!dir.exists(here("datadownloads/dsscps"))) 
{dir.create(here("datadownloads/dsscps"))}

walk2(urls, dest, download.file, method = "curl", extra = "-k")


# read in files ----
# test <- read_excel(dest[12], sheet = 1)

cps_data <- map(dest, ~read_excel(.x, sheet = 1))
names(cps_data) <- 2020:2009 # add year as names for list

# rename columns to be identical across years
colnames = c("locality","region","screen_out", "accepted", "no_select",
             "tot_ref", "assess_serv", "assess_noserv", "assess_other",
             "assess_total", "level1", "level2", "level3", "founded",
             "unfounded", "other_inv", "total_inv", "pending") 

cps_data <- map(cps_data, ~ rename_with(., ~ colnames))

# bind into data frame
cps_all <- bind_rows(cps_data, .id = "year")


# Pull cville, alb data ----
cps <- cps_all %>% 
  filter(locality %in% c("Albemarle", "Charlottesville")) %>% 
  select(year, locality, cps_count = accepted) %>% 
  mutate(year = as.numeric(year),
         fips = ifelse(locality == "Albemarle", "51003", "51540"))


# Create state data and bind to localities ----
cps_va <- cps_all %>% 
  select(year, locality, accepted) %>% 
  group_by(year) %>% 
  summarize(cps_count = sum(accepted)) %>% 
  mutate(locality = "Virginia", fips = "51",
         year = as.numeric(year))

cps <- bind_rows(cps, cps_va)

# Pull in population data, join, create percent ----
pop <- read_csv("data/pop_data_cdc.csv") %>% 
  mutate(fips = as.character(fips)) %>% 
  select(fips, year, pop_17under)

# join pop to foster care
cps <- cps %>% 
  left_join(pop)

# create rate per 1000
cps <- cps %>% 
  mutate(cps_rate = (cps_count/pop_17under)*1000)

# have a peek
ggplot(cps, aes(x = year, y = cps_rate, color = locality)) +
  geom_line()


# save data ----
write_csv(cps, "data/child_welfare_reports.csv")
# cps <- read_csv("data/child_welfare_reports.csv")


# Note: DSS has several other reports (in pdf form)
# https://www.dss.virginia.gov/geninfo/reports/children/cps/all_other.cgi
# Referrals and Findings (by locality) is closest to data from prior report
# but does not exactly match data from the Annual Summary used here;
# the Referrals and Findings is not available before 2008.
# In addition, different reports from the same year that reference
# referrals and accepted referrals provide different numbers; 
# for example, Abuse & Neglect (by locality) shows accepted referrals by
# allegations and sums to more than accepted referrals in the prior report
# (presumably because a report contains more than one allegation and each 
# allegation is summed in the Abuse & Neglect report - a count of allegations versus a count of referrals).
# In 2007 and before, the Abuse & Neglect Completed Reports (by locality) appear to be similar,
# though need to be careful not to inadvertently use counts of children versus reports.