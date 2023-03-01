# Stepping Stones Data: Child Poverty ----
# Updated 2023-01-15
# Contributor: Michele Claibourn
# Generate child poverty estimates
#
# Based on Census Small Area Income and Poverty Estimates
# https://www.census.gov/programs-surveys/saipe/data/datasets.html
# Charlottesville, Albemarle, State
#
# Choose State and County Estimates
# Copy link for Virginia: 
# https://www2.census.gov/programs-surveys/saipe/datasets/2021/2021-state-and-county/est21-va.txt
# 
# Proposed Citation: 
# U.S. Census Bureau, "SAIPE State and County Estimates for 2021."  
# https://www.census.gov/programs-surveys/saipe/data/datasets.html
#
# Alternative estimates available in ACS (2009-2021)
# See in-class example code (2/2)


# Libraries ----
library(tidyverse)


# Read data ----
# ## Examine 2021 ----
# url21 <- "https://www2.census.gov/programs-surveys/saipe/datasets/2021/2021-state-and-county/est21-va.txt"
# 
# saipe21 <- read_fwf(url21,
#                     fwf_cols(state_fips = c(1, 2), county_fips = c(4,6),
#                              num_child_pov = c(50,57), pct_child_pov = c(77,80),
#                              num_child_pov_lb = c(59,66), num_child_pov_ub = c(68,75),
#                              pct_child_pov_lb = c(82,85), pct_child_pov_ub = c(87,90)),
#                     col_types = "ccnnnnnn")
# 
# cpov21 <- saipe21 %>% 
#   filter(county_fips %in% c("0", "3", "540")) %>% 
#   mutate(county_fips = str_pad(county_fips, width = 3, side = "left", pad = "0"),
#          year = 2021) %>% 
#   select(county_fips, year, num_child_pov, pct_child_pov)


# Make a function ----
# load saipe and generate childpov
# arguments: url, year, countyfips
extract_childpov <- function(url, year, countyfips){
  saipe <- read_fwf(url,
                      fwf_cols(state_fips = c(1, 2), county_fips = c(4,6),
                               num_child_pov = c(50,57), pct_child_pov = c(77,80),
                               num_child_pov_lb = c(59,66), num_child_pov_ub = c(68,75),
                               pct_child_pov_lb = c(82,85), pct_child_pov_ub = c(87,90)),
                      col_types = "ccnnnnnn")
  
  cpov <- saipe %>% 
    filter(county_fips %in% c("0", countyfips)) %>% 
    mutate(county_fips = str_pad(county_fips, width = 3, side = "left", pad = "0"),
           year = year) %>% 
    select(county_fips, year, num_child_pov, pct_child_pov)
  
  return(cpov)
}

# Create list of urls ----
# they follow a clear pattern where only the value of the year shifts
# (except 2000-2003 have .dat extensions; 2003-2021 have .txt)
years <- tibble(y = seq(0,21,1), x = rep(c("dat", "txt"), c(4, 18))) %>% 
  mutate(y = as.character(y),
         y = str_pad(y, 2, side = "left", pad = "0"))
urllist <- paste0("https://www2.census.gov/programs-surveys/saipe/datasets/20",years$y,"/20",years$y,"-state-and-county/est",years$y,"-va.",years$x)


# Apply function across 2000-2021 ----
# make a data frame of paired url and year arguments
url_year <- tibble(url = urllist, year = years$y) %>% 
  mutate(year = paste0("20",year), 
         year = as.numeric(year))

# apply function across the url/year pairs, add countyfips argument
cpov_list <- map2(url_year$url, url_year$year, 
                  ~extract_childpov(.x, .y, countyfips = c("3", "540"))) 

# bind the list of resulting data frames into a single dataframe
cpov <- bind_rows(cpov_list)

# And have a peek
ggplot(cpov, aes(x = year, y = pct_child_pov, color = county_fips)) +
  geom_line()


# Save data ----
write_csv(cpov, "data/child_pov.csv")
