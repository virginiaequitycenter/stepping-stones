# Title: Stepping Stones Project
# Point-in-Time Count of People Experiencing Homelessness by race
# Michele Claibourn
# Last Updated: 2023-06-06

# Proposed Citation
# Blue Ridge Area Coalition for the Homeless, "Point-in-Time Count", 2014-2022.

# ..................................................
# Load Libraries ----

library(tidyverse)
library(readxl)
library(janitor)
library(tidycensus)
# library(lubridate)


# ..................................................
# Generate list of files, read ----
pitlist <- list.files(path = "datadownloads/All Reports", recursive = TRUE, pattern = "General", full.names = TRUE)
# remove 2012 and 2013
pitlist <- pitlist[3:12]

# Read in  ----
# slightly different formats in 2014-2016, 2017-2021, 2022-2023
## 2014-2016 ----
pit1416 <- map(pitlist[1:3], ~read_excel(.x, sheet = "Totals"))
names(pit1416) <- 2014:2016 # add year as names for list
pit1416 <- map(pit1416, ~rename(., group = 1))
pit1416 <- bind_rows(pit1416, .id = "year") %>% 
  rename(es = 3, ts = 4, sh = 5, un = 6, total = 7) %>% 
  filter(group %in% c("Total Number of \nPersons", 
                      "Non-Hispanic/Non-Latino", "Hispanic/Latino",
                      "White", "Black or African-American", 
                      "Asian", "American Indian or Alaska Native", 
                      "Native Hawaiian or Other Pacific Islander", "Multiple Races"))

## 2017-2021 ----
pit1721 <- map(pitlist[4:8], ~read_excel(.x, sheet = "Totals"))
names(pit1721) <- 2017:2021 # add year as names for list
pit1721 <- map(pit1721, ~rename(., group = 1))
pit1721 <- bind_rows(pit1721, .id = "year") %>% 
  select(year, group, es = 4, ts = 6, sh = 8, un = 10, total = 12) %>% 
  filter(group %in% c("Total Number of \nPersons", 
                      "Non-Hispanic/Non-Latino", "Hispanic/Latino",
                      "White", "Black or African-American", 
                      "Asian", "American Indian or Alaska Native", 
                      "Native Hawaiian or Other Pacific Islander", "Multiple Races"))

## 2022-2023 ----
pit2223 <- map(pitlist[9:10], ~read_excel(.x, sheet = "Totals"))
names(pit2223) <- 2022:2023 # add year as names for list
pit2223 <- map(pit2223, ~rename(., group = 1))
pit2223 <- bind_rows(pit2223, .id = "year") %>% 
  select(year, group, es = 4, ts = 6, sh = 8, un = 10, total = 12) %>% 
  filter(group %in% c("Total Number of \nPersons", 
                      "Non-Hispanic/Non-Latin(a)(o)(x)", "Hispanic/Latin(a)(o)(x)",
                      "White", "Black, African American, or African", 
                      "Asian or Asian American", "American Indian, Alaska Native, or Indigenous", 
                      "Native Hawaiian or Pacific Islander", "Multiple Races"))

# ..................................................
# Combine and standardize race ----
pit_race <- bind_rows(pit1416, pit1721, pit2223)

pit_race<- pit_race %>% 
  mutate(initialgroup = group,
         group = fct_collapse(group,
                               white = "White",
                               black = c("Black or African-American", "Black, African American, or African"),
                               asian = c("Asian", "Asian or Asian American"),
                               aian = c("American Indian or Alaska Native", "American Indian, Alaska Native, or Indigenous"),
                               nhpi = c("Native Hawaiian or Pacific Islander", "Native Hawaiian or Other Pacific Islander"),
                               multi = "Multiple Races",
                               hispanic = c("Hispanic/Latino", "Hispanic/Latin(a)(o)(x)"),
                               nonhispanic = c("Non-Hispanic/Non-Latino", "Non-Hispanic/Non-Latin(a)(o)(x)"),
                               total = "Total Number of \nPersons")) %>% 
  mutate(total = as.numeric(total)) %>% 
  group_by(year, group) %>% 
  summarize(total = sum(total))


# ..................................................
# Read in population data & prep ----
## Charlottesville and Albemarle totals ----
# race: b02001
# hispanic: b03003
region <- c("540", "003")
years <- 2014:2021
vars <- c(white = "B02001_002", 
          black = "B02001_003", 
          aian = "B02001_004",
          asian = "B02001_005",
          nhpi = "B02001_006", 
          other = "B02001_007", 
          multi = "B02001_008")

race_cvlalb <- map_dfr(years,
                ~get_acs(geography = "county",
                      state = "51",
                      county = region,
                      var = vars,
                      year = .x,
                      survey = "acs5") %>%
                  mutate(year = .x))

varsb <- c(total = "B03003_001", 
          nonhispanic = "B03003_002", 
          hispanic = "B03003_003")

hisp_cvlalb <- map_dfr(years,
                ~get_acs(geography = "county",
                         state = "51", 
                         county = region,
                         var = varsb,
                         year = .x,
                         survey = "acs5") %>% 
                  mutate(year = .x))

pop_race_cvlalb <- race_cvlalb %>% 
  group_by(year, variable) %>% 
  summarize(pop_cvlalb = sum(estimate)) %>% 
  rename(group = variable)

pop_hisp_cvlalb <- hisp_cvlalb %>% 
  group_by(year, variable) %>% 
  summarize(pop_cvlalb = sum(estimate)) %>% 
  rename(group = variable)

pop_cvlalb <- bind_rows(pop_hisp_cvlalb, pop_race_cvlalb)


## Six-county region totals ----
region <- c("540", "003", "065", "079", "109", "125")

race_region <- map_dfr(years,
                       ~get_acs(geography = "county",
                                state = "51",
                                county = region,
                                var = vars,
                                year = .x,
                                survey = "acs5") %>%
                         mutate(year = .x))

hisp_region <- map_dfr(years,
                       ~get_acs(geography = "county",
                                state = "51", 
                                county = region,
                                var = varsb,
                                year = .x,
                                survey = "acs5") %>% 
                         mutate(year = .x))

pop_race_region <- race_region %>% 
  group_by(year, variable) %>% 
  summarize(pop_region = sum(estimate)) %>% 
  rename(group = variable)

pop_hisp_region <- hisp_region %>% 
  group_by(year, variable) %>% 
  summarize(pop_region = sum(estimate)) %>% 
  rename(group = variable)

pop_region <- bind_rows(pop_hisp_region, pop_race_region)

# ..................................................
# Join population data, create rate(s) ----
pit_pop <- pit_race %>% 
  mutate(year = as.integer(year)) %>% 
  left_join(pop_cvlalb) %>% 
  left_join(pop_region) %>% 
  ## fill in 2022 population with 2021 estimates
  group_by(group) %>%
  fill(pop_cvlalb, pop_region) %>%
  ungroup() %>% 
  mutate(rate_cvlalb = (total/pop_cvlalb)*1000,
         rate_region = (total/pop_region)*1000) %>% 
  # remove 2023 due to absence of population data
  filter(year != 2023)


# ..................................................
# Visualize and save ----

ggplot(pit_pop, aes(x = year, y = rate_cvlalb, color = group)) +
  geom_line()

ggplot(pit_pop, aes(x = year, y = rate_region, color = group)) +
  geom_line()

## save ----
write_csv(pit_pop, "data/pit_homelessness_race.csv")
# pit_pop <- read_csv("data/pit_homelessness_race.csv")
