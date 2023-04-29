# Title: Stepping Stones Project
# Finance-Youth Unemployment, Labor Force Participation
# Team 5: Owayne Owens, Amruta Binoy, Mary Katherine West; Michele Claibourn
# Last Updated: 2023-03-05

# Proposed Citation: 
# U.S. Census Bureau, American Community Survey 5-year estimates, 
# "Sex by Age by Employment Status (B23001)." 2010-2021. 
# https://data.census.gov/table?q=b23001&g=0500000US51003,51540


# ..................................................
# Set up ----

# Load Libraries
library(tidyverse)
library(janitor)
library(tidycensus)

# ..................................................
# Reading in census data: Alb, Cvl ----

# find vars
# detail_vars <- load_variables(2021, "acs5", cache = TRUE)
# view(detail_vars)
# B23001_004 - male 16-19 in labor force 
# B23001_008 - male 16-19 unemployed 
# B23001_090 - female 16-19 in labor force 
# B23001_094 - female 16-19 unemployed

total_vars <- c(total_males = "B23001_003", total_females = "B23001_089") 
labor_vars <- c(male_labor = "B23001_004", female_labor = "B23001_090")
unemployed_vars <- c(male_unemployed  = "B23001_008", female_unemployed = "B23001_094")
years <- 2010:2021

## read in
youth_total <- map_dfr(years, 
                       ~get_acs(
                                  geography = "county",
                                  state = "51",
                                  county = c("003", "540"),
                                  year = .x, #refers to an element of a list 
                                  survey = "acs5",
                                  var = total_vars,
                                  output = "tidy") %>%
                                  mutate(year = .x))
youth_labor <- map_dfr(years, 
                       ~get_acs(
                         geography = "county",
                         state = "51",
                         county = c("003", "540"),
                         year = .x, #refers to an element of a list 
                         survey = "acs5",
                         var = labor_vars,
                         output = "tidy") %>%
                         mutate(year = .x))

youth_unemployed <- map_dfr(years, 
                       ~get_acs(
                         geography = "county",
                         state = "51",
                         county = c("003", "540"),
                         year = .x, #refers to an element of a list 
                         survey = "acs5",
                         var = unemployed_vars,
                         output = "tidy") %>%
                         mutate(year = .x))


# ..................................................
# Prepping data: Alb, Cvl ----

## Calculating sum of overall youth, those in labor force and youth unemployed 
## clean names
youth_total <- clean_names(youth_total)
youth_labor <- clean_names(youth_labor)
youth_unemployed <- clean_names(youth_unemployed)

## sum by locality, year 
youth_total_sum <- youth_total %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_total_sum = sum(estimate)) %>% 
  ungroup()

youth_labor_sum <- youth_labor %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_labor_sum = sum(estimate)) %>% 
  ungroup()

youth_unemployed_sum <- youth_unemployed %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_unemployed_sum = sum(estimate)) %>% 
  ungroup()

# Joining 
youth_unemployed_sum <- youth_unemployed_sum %>% 
  left_join(youth_labor_sum, by = c("geoid", "name", "year"))
youth_labor_sum <- youth_labor_sum %>% 
  left_join(youth_total_sum, by = c("geoid", "name", "year"))

# Unemployment and participation rates
youth_unemployed_sum <- youth_unemployed_sum %>% 
  mutate(unemployment_rate = youth_unemployed_sum/youth_labor_sum * 100)
youth_labor_sum <- youth_labor_sum %>% 
  mutate(laborforce_rate = youth_labor_sum/youth_total_sum * 100)


# ..................................................
# Repeat for states  ----

# read in
youth_total_state <- map_dfr(years, 
                             ~get_acs(
                               geography = "state",
                               state = "51",
                               year = .x, 
                               survey = "acs5",
                               var = total_vars,
                               output = "tidy") %>%
                               mutate(year = .x))

youth_labor_state <- map_dfr(years, 
                             ~get_acs(
                               geography = "state",
                               state = "51",
                               year = .x,
                               survey = "acs5",
                               var = labor_vars,
                               output = "tidy") %>%
                               mutate(year = .x))

youth_unemployed_state <- map_dfr(years, 
                                  ~get_acs(
                                    geography = "state",
                                    state = "51",
                                    year = .x,
                                    survey = "acs5",
                                    var = unemployed_vars,
                                    output = "tidy") %>%
                                    mutate(year = .x))

# ..................................................
# Prepping data: Virginia  ----
# clean names
youth_total_state <- clean_names(youth_total_state)
youth_labor_state <- clean_names(youth_labor_state)
youth_unemployed_state <- clean_names(youth_unemployed_state)

# Summing up 
youth_sum_state <- youth_total_state %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_total_sum = sum(estimate)) %>% 
  ungroup()

youth_labor_state_sum <- youth_labor_state %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_labor_sum = sum(estimate)) %>% 
  ungroup()

youth_unemployed_state_sum <- youth_unemployed_state %>% 
  group_by(geoid, name, year) %>% 
  summarize(youth_unemployed_sum = sum(estimate)) %>% 
  ungroup()

# Joining 
youth_unemployed_state_sum <- youth_unemployed_state_sum %>% 
  left_join(youth_labor_state_sum, by = c("geoid", "name", "year"))
youth_labor_state_sum <- youth_labor_state_sum %>% 
  left_join(youth_sum_state, by = c("geoid", "name", "year")) 

# Labor force participation rate and Unemployment rate
youth_unemployed_state_sum <- youth_unemployed_state_sum %>% 
  mutate(unemployment_rate = youth_unemployed_sum/youth_labor_sum * 100)
youth_labor_state_sum <- youth_labor_state_sum %>% 
  mutate(laborforce_rate = youth_labor_sum/youth_total_sum * 100)



# ..................................................
# Combine locality and state ----
unemployment <- bind_rows(youth_unemployed_sum, youth_unemployed_state_sum)
laborforce <- bind_rows(youth_labor_sum, youth_labor_state_sum)

employmentstatus <- unemployment %>% 
  left_join(laborforce)


# ..................................................
## Saving the data ----

write_csv(employmentstatus, "data/youth_employment_status.csv")
# employmentstatus <- read_csv("data/youth_employment_status.csv")

# have a peek
ggplot(employmentstatus, aes(x = year, y = laborforce_rate, color = name)) +
  geom_line()
ggplot(employmentstatus, aes(x = year, y = unemployment_rate, color = name)) +
  geom_line()
