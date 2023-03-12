# Title: Stepping Stones Project
# Housing-Renters Paying More than 30% of Income for Housing (%) 
# Team 2: Ramya, Michael, Kate; Michele Claibourn
# Last Updated: 2023-03-07

# Proposed Citation
# American Community Survey, 5-Year Survey Estimates, Table B25106, "Tenure by Housing Costs as a Percentage of Household Income in the Past 12 Months", 2009-2021. 
# Accessed through tidycensus


# ..................................................
# Load libraries ----
library(tidyverse)
library(tidycensus)

# ..................................................
# Find variables ----

#Examine acs table for variable number
detail_vars <- load_variables(2021, "acs5", cache = TRUE)

#Assign data of interest to new variable
#B25106_006 - Owner-occupied housing units that cost less than $20,000
#B25106_010 - Owner-occupied housing units that cost 20,000 to $34,999
#B25106_014 - Owner-occupied housing units that cost $35,000 to $49,999
#B25106_018 - Owner-occupied housing units that cost $50,000 to $74,999
#B25106_022 - Owner-occupied housing units that cost $75,000 or more
#B25106_002 - Owner-occupied housing units (total)


# ..................................................
# Pull variables, Charlottesville/Albemarle ----
vars <- c(hhibelow20k = "B25106_006",
                     hhi20kto34k = "B25106_010",
                     hhi35kto49k = "B25106_014",
                     hhi50kto74k = "B25106_018",
                     hhi75kabove = "B25106_022",
          hh_all = "B25106_002")

years <- 2010:2021

homeburden <- map_dfr(years,
                ~get_acs(
                  geography = "county",
                  state = "51",
                  county = c("003", "540"),
                  year = .x,
                  survey = "acs5",
                  var = vars,
                  output = "tidy") %>%
                  mutate(year = .x)
)


# ..................................................
# Prep variables: Charlottesville/Albemarle ----
## extract total number of owner occupied households
hh_all <- homeburden %>% 
  filter(variable == "hh_all") %>% 
  select(GEOID, NAME, total_hh = estimate, year)

## generate total number of burdened owner occupied households (and percent)
hh_burden <- homeburden %>% 
  filter(variable != "hh_all") %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(burden_hh = sum(estimate))

## combine burdened and totals and create percent
hh <- hh_burden %>% 
  left_join(hh_all) %>% 
  mutate(pct_burden = (burden_hh/total_hh)*100)


# ..................................................
# Pull variables, State ----
homeburden_state <- map_dfr(years,
                      ~get_acs(
                        geography = "state",
                        state = "51",
                        year = .x,
                        survey = "acs5",
                        var = vars,
                        output = "tidy") %>%
                        mutate(year = .x)
)


# ..................................................
# Prep variables: State ----
## extract total number of owner occupied households
hh_all_state <- homeburden_state %>% 
  filter(variable == "hh_all") %>% 
  select(GEOID, NAME, total_hh = estimate, year)

## generate total number of burdened owner occupied households (and percent)
hh_burden_state <- homeburden_state %>% 
  filter(variable != "hh_all") %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(burden_hh = sum(estimate))

## combine burdened and totals and create percent
hh_state <- hh_burden_state %>% 
  left_join(hh_all_state) %>% 
  mutate(pct_burden = (burden_hh/total_hh)*100)


# ..................................................
# Combine divisions, state ----
hh_combined <- bind_rows(hh, hh_state)

# have a peek
ggplot(hh_combined, aes(x = year, y = pct_burden, color = NAME)) +
  geom_line() +
  geom_point()


# ..................................................
# Save ----
write_csv(hh_combined, "data/homeowner_burden.csv")
