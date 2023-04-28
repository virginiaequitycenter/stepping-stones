# Stepping Stones Data: Per Capita Income ----
# Updated 2023-01-14
# Contributors: Michele Claibourn
# Generate per capita income estimates
# Based on Bureau of Economic Analysis data
# https://www.bea.gov/data/economic-accounts/regional
# Charlottesville, Albemarle, State

# Sources ----
# MSA Per Capita Income measures 
#    https://apps.bea.gov/iTable/?reqid=70&step=1&isuri=1&acrdn=6
#    choosing "Personal income and employment by county and metropolitan area"
#    choosing "County"
#    choosing "Virginia"
#    choosing Area = "Albemarle + Charlottesvill, VA"
#             Statistic = "All statistics in table"
#             Units of measure = "Levels"
#             Time period = "All years"
# Generates table url (exported to datadownloads manually)
# https://apps.bea.gov/iTable/?reqid=70&step=1&isuri=1&acrdn=6#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIyMCJdLFsiQ2xhc3NpZmljYXRpb24iLCJOb24tSW5kdXN0cnkiXSxbIk1ham9yX0FyZWEiLCI0Il0sWyJTdGF0ZSIsWyI1MTAwMCJdXSxbIkFyZWEiLFsiNTE5MDEiXV0sWyJTdGF0aXN0aWMiLFsiLTEiXV0sWyJVbml0X29mX21lYXN1cmUiLCJMZXZlbHMiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCItMSJdLFsiWWVhcl9FbmQiLCItMSJdXX0=
# 
# Suggested Citation from page: 
# U.S. Bureau of Economic Analysis, "CAINC1 County and MSA personal income summary: personal income, population, per capita personal income" (accessed Saturday, January 14, 2023).
# 
# State Per Capita Income measures
#    https://apps.bea.gov/iTable/?reqid=70&step=1&isuri=1&acrdn=6
#    choosing "Summary table for GDP, personal income, and related data"
#    choosing Area = "Virginia", 
#             Statistic = "Personal Income" & "Per capita personal income"
#             Units of measure = "Levels"
#             Time period = "All years"
# Generates table url (exported to datadownloads manually)
# https://apps.bea.gov/iTable/?reqid=70&step=1&isuri=1&acrdn=6#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI2MDAiXSxbIkNsYXNzaWZpY2F0aW9uIiwiTm9uLUluZHVzdHJ5Il0sWyJNYWpvcl9BcmVhIiwiMCJdLFsiU3RhdGUiLFsiMCJdXSxbIkFyZWEiLFsiNTEwMDAiXV0sWyJTdGF0aXN0aWMiLFsiNSIsIjEwIl1dLFsiVW5pdF9vZl9tZWFzdXJlIiwiTGV2ZWxzIl0sWyJZZWFyIixbIi0xIl1dLFsiWWVhckJlZ2luIiwiLTEiXSxbIlllYXJfRW5kIiwiLTEiXV19
#
# Suggested Citation from page: 
# U.S. Bureau of Economic Analysis, "SASUMMARY State annual summary statistics: personal income, GDP, consumer spending, price indexes, and employment" (accessed Saturday, January 14, 2023). 
# https://www.bea.gov/data/economic-accounts/regional
#
# Consumer Price Index measure
# https://www.bls.gov/cpi/data.htm
# CPI-All Urban Consumers from BLS
# Top Picks --> U.S City Average, All Items; Choose More formatting options; 
# Choose "table format" and "Original data value";
# Specify year range (2000-2022), Select "One time period" = "Annual Data"
# Output type = "HTML table"; Retrieve; Download xlsx
# No suggested citation, but how about:
# U.S. Bureau of Labor Statistics, "All items in U.S. city average, all urban consumers, not seasonally adjusted", Series Id CUUR0000SA0 (accessed Saturday, January 14, 2023). 


# Libraries ----
library(tidyverse)
library(readxl)


# Read in downloaded data ----
cvlalb_table <- read_csv("datadownloads/cvlalb_pci_table.csv", skip = 3, n_max = 3)
state_table <- read_csv("datadownloads/state_pci_table.csv", skip = 3, n_max = 3)


# Pivot and combine msa and state tables ----
# pivot longer
clvalb <- cvlalb_table %>% 
  pivot_longer(-c(GeoFips, GeoName, LineCode, Description), names_to = "year", values_to = "var")

state <- state_table %>% 
  pivot_longer(-c(GeoFips, GeoName, LineCode, Description), names_to = "year", values_to = "var")

# # check
# clvalb %>% filter(LineCode == 3, year >= 2000) %>%
#   ggplot(aes(x = year, y = var)) +
#   geom_line(group = 1)
# 
# state %>% filter(LineCode == 10, year >= 2000) %>% 
#   ggplot(aes(x = year, y = var)) +
#   geom_line(group = 1)

# combine msa and state
cvlalb_pci <- clvalb %>% 
  filter(LineCode == 3, year >= 2000) %>% 
  select(GeoFips, GeoName, year, pci = var)

state_pci <- state %>% 
  filter(LineCode == 10, year >= 2000) %>% 
  select(GeoFips, GeoName, year, pci = var)
  
pci <- bind_rows(cvlalb_pci, state_pci)

pci <- pci %>% 
  mutate(GeoFips = as.character(GeoFips),
         year = as.numeric(year))


# Adjust for inflation ----
# A useful review on inflation adjustment: https://timeseriesreasoning.com/contents/inflation-adjustment/
# Starting with CPI-All Urban Consumers from BLS
# https://www.bls.gov/cpi/data.htm
# Top Picks --> U.S City Average, All Items
# Choose From 2000 to 2022; Select include annual averages; Click Go; Download xlsx
# OR
# Choose More formatting options; Choose table format and Original data value;
# Specify year range (2000-2022), Select one time period = Annual Data
# Output type = HTML table; Retrieve; Download xlsx
# No suggested citation, but how about:
# U.S. Bureau of Labor Statistics, "All items in U.S. city average, all urban consumers, not seasonally adjusted", Series Id CUUR0000SA0 (accessed Saturday, January 14, 2023). 

# read in downloaded data
bls_ann <- read_excel("datadownloads/bls_cpi_annual_20002022.xlsx", skip = 10)

# Revise to base year 2010
# (Default is 1982-1984)
base2010 <- as_vector(bls_ann %>% filter(Year == 2010) %>% select(Annual))

bls_ann <- bls_ann %>% 
  mutate(cpi10 = Annual/base2010*100)

# Add this to pci
pci <- pci %>% 
  left_join(bls_ann %>% select(cpi10, Year), by = c("year" = "Year"))

pci <- pci %>% 
  mutate(adj_pci = pci/cpi10*100)

# Have a look
ggplot(pci, aes(x = year, y = pci, color = GeoFips, group = GeoFips)) +
  geom_line() +
  geom_line(data = pci, aes(x = year, y = adj_pci, color = GeoFips, group = GeoFips), linetype = "dashed") +
  theme(legend.position = "bottom")

# Probably need to pivot this longer to use pci, adj_pci as an aesthetic...


# Save data ----
write_csv(pci, "data/per_cap_income.csv")


# Notes ----
# County measures (versus MSA measures) are actually easier to retrieve
# County measures
# https://apps.bea.gov/regional/downloadzip.cfm
# Choosing Personal Income (State and Local) --> CAINC1: Annual Personal Income by County
# and Download
#
# State measures
# https://apps.bea.gov/regional/downloadzip.cfm
# Choosing Personal Income (State and Local) --> Annual Personal Income and Employment by State
# and Download
# 
# Comparing MSA and County measures 
# (combined county estimates for Cvl/Alb are higher than larger MSA)
# Using County measures
#
# A useful review on inflation adjustment: https://timeseriesreasoning.com/contents/inflation-adjustment/
# CPI background: https://webapps.dol.gov/dolfaq/go-dol-faq.asp?faqid=98&topicid=6 

