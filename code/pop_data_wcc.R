# Stepping Stones Data: Population totals ----
# Updated 2023-02-03
# Contributor: Michele Claibourn
# Generate locality and state population estimates totals
# From Cooper Center estimates
# https://demographics.coopercenter.org/virginia-population-estimates

# Citation from page: 
# University of Virginia Weldon Cooper Center, Demographics Research Group. (2020). 
# Virginia Population Estimates. Retrieved from https://demographics.coopercenter.org/virginia-population-estimates

# Libraries ----
library(tidyverse)
library(readxl)
library(janitor)

# Download/read data ----
wcc0010url <- "https://demographics.coopercenter.org/sites/demographics/files/VA-Intercensal-Estimates_2000-2010.xls"
wcc1020url <- "https://demographics.coopercenter.org/sites/demographics/files/media/files/2023-01/VA-Intercensal-Estimates_2010-2020_UVA-CooperCenter_Updated-2023-01.xlsx"
wcc2022url <- "https://demographics.coopercenter.org/sites/demographics/files/media/files/2023-01/VA-Intercensal-Estimates_2020-2022_UVA-CooperCenter.xlsx"

download.file(wcc0010url, "datadownloads/pop_wcc_0010.xls")
download.file(wcc1020url, "datadownloads/pop_wcc_1020.xlsx")
download.file(wcc2022url, "datadownloads/pop_wcc_2022.xlsx")

wcc0010 <- read_excel("datadownloads/pop_wcc_0010.xls", skip = 3)
wcc1020 <- read_excel("datadownloads/pop_wcc_1020.xlsx", skip = 4)
wcc2022 <- read_excel("datadownloads/pop_wcc_2022.xlsx", skip = 4)

# Filter to localities ----
wcc0010 <- wcc0010 %>% 
  rename(fips = `...1`, locality = `...2`, census00 = `...3`, census10 = `...14`) %>% 
  select(fips, locality, `2000`:`2009`) %>% 
  filter(locality %in% c("Charlottesville City", "Albemarle County", "Virginia")) %>% 
  mutate(fips = ifelse(locality == "Virginia", "51", fips))

wcc1020 <- wcc1020 %>% 
  rename(fips = `...1`, locality = `...2`, census10 = `...3`) %>% 
  select(fips, locality, `2010`:`2020`) %>% 
  filter(locality %in% c("Charlottesville City", "Albemarle County", "Virginia")) %>% 
  mutate(fips = ifelse(locality == "Virginia", "51", fips))

wcc2022 <- wcc2022 %>% 
  rename(fips = `...1`, locality = `...2`, census20 = `...3`) %>% 
  select(fips, locality, `2020`:`2022`) %>% 
  filter(locality %in% c("Charlottesville City", "Albemarle County", "Virginia")) %>% 
  mutate(fips = ifelse(locality == "Virginia", "51", fips))

# Reshape ----
wcc0010long <- wcc0010 %>% 
  pivot_longer(cols = -c(fips, locality), names_to = "year", values_to = "pop_wcc")

wcc1020long <- wcc1020 %>% 
  pivot_longer(cols = -c(fips, locality), names_to = "year", values_to = "pop_wcc")

wcc2022long <- wcc2022 %>% 
  pivot_longer(cols = -c(fips, locality), names_to = "year", values_to = "pop_wcc")

# Bind (and dedupe) ----
# 2020 appears in both wcc1020 and wcc2022
wcc1020long <- wcc1020long %>% 
  filter(year != "2020")

pop_wcc <- bind_rows(wcc0010long, wcc1020long, wcc2022long)

pop_wcc <- pop_wcc %>% 
  arrange(locality, year)

# Save ----
write_csv(pop_wcc, "data/pop_data_wcc.csv")
