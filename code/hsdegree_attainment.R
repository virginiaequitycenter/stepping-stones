# Stepping Stones Project 
# Education-Residents Over the Age 25 with High School Diploma/Equivalent or Higher (%)  
# Team 1 Fara, Lekha, & Lily 
# Last Updated: Feb 14, 2023 

# Proposed Citation: 
# U.S. Census Bureau, American Community Survey 5-year estimates, 
# "Educational Attainment (S1501)." 2010-2021. 
# https://data.census.gov/table?q=S1501&tid=ACSST1Y2021.S1501

# ..................................................

# Document set up ----

# Load libraries
library(tidyverse)
library(tidycensus)

# ..................................................

# Reading in census data ----
subject_vars_14 <- load_variables(2014, "acs5/subject", cache = TRUE)

subject_vars_17 <- load_variables(2017, "acs5/subject", cache = TRUE)

subject_vars_21 <- load_variables(2021, "acs5/subject", cache = TRUE)

## Charlottesville & Albemarle data  ----

### 2010 - 2014 data ----
years_10_14 <- 2010:2014

vars_10_14 <- c(pcthshigher = "S1501_C01_014",
                totpop25 = "S1501_C01_006")

hs_higher_pop_cville_alb_10_14 <- map_dfr(years_10_14,
                                          ~get_acs(
                                            geography = "county",
                                            state = "51",
                                            county = c("003", "540"),
                                            year = .x,  
                                            survey = "acs5",
                                            var = vars_10_14,
                                            output = "wide") %>%
                                            mutate(year = .x)) %>%
  select(-pcthshigherM, -totpop25M)

### 2015-2021 data ----
years_15_21 <- 2015:2021

vars_15_21 <- c(pcthshigher = "S1501_C02_014",
                totpop25 = "S1501_C01_006")

hs_higher_pop_cville_alb_15_21 <- map_dfr(years_15_21,
                                          ~get_acs(
                                            geography = "county",
                                            state = "51",
                                            county = c("003", "540"),
                                            year = .x,  
                                            survey = "acs5",
                                            var = vars_15_21,
                                            output = "wide") %>%
                                            mutate(year = .x)) %>%
  select(-pcthshigherM, -totpop25M)

### Combine 2010 - 2021 data ----
hs_higher_pop_cville_alb_10_21 <- bind_rows(hs_higher_pop_cville_alb_10_14, 
                                            hs_higher_pop_cville_alb_15_21) 


## Virginia data  ----

### 2010 - 2014 data ----
hs_higher_pop_va_10_14 <- map_dfr(years_10_14,
                                  ~get_acs(
                                    geography = "state",
                                    state = "51",
                                    year = .x,  
                                    survey = "acs5",
                                    var = vars_10_14,
                                    output = "wide") %>%
                                    mutate(year = .x)) %>%
  select(-pcthshigherM, -totpop25M)

### 2015-2021 data ----
hs_higher_pop_va_15_21 <- map_dfr(years_15_21,
                                  ~get_acs(
                                    geography = "state",
                                    state = "51",
                                    year = .x,  
                                    survey = "acs5",
                                    var = vars_15_21,
                                    output = "wide") %>%
                                    mutate(year = .x)) %>%
  select(-pcthshigherM, -totpop25M)

### Combine 2010 - 2021 data ----
hs_higher_pop_va_10_21 <- bind_rows(hs_higher_pop_va_10_14, 
                                    hs_higher_pop_va_15_21) 

# ..................................................

# Combine county & state data ---- 
hs_higher_pop_10_21 <- bind_rows(hs_higher_pop_va_10_21, 
                                 hs_higher_pop_cville_alb_10_21) 

# have a peek
ggplot(hs_higher_pop_10_21, aes(x = year, y = pcthshigherE, color = NAME)) +
  geom_line()

# ..................................................

# Export data  ----
write_csv(hs_higher_pop_10_21, "data/hsdegree_attainment.csv")
