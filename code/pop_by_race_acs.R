# Stepping Stones Data: Child population/race ----
# Updated 2023-06-12
# Contributor: Lee LeBoeuf & Michele Claibourn 
# Generate locality and state child population estimates by race
#   used to create percents or rates for multiple metrics
# Using American Community Survey, 5-year estimates
# Table B01001
# Charlottesville, Albemarle, State

# Libraries ----
library(tidyverse)
library(janitor)
library(tidycensus)


# Download ----
years <- 2009:2021

## pull county data ----
vars <- c(white = "B01001A_001",
          black = "B01001B_001",
          aian = "B01001C_001",
          asian = "B01001D_001",
          nhpi = "B01001E_001",
          other = "B01001F_001",
          multi = "B01001G_001",
          nhwhite = "B01001H_001",
          hispanic = "B01001I_001")

pop <- map_dfr(years,
               ~get_acs(
                 geography = "county",
                 state = "51",
                 county = c("003", "540"),
                 var = vars,
                 year = .x,
                 survey = "acs5") %>%
                 mutate(year = .x)
)

# repeat for state
pop_state <- map_dfr(years,
                     ~get_acs(
                       geography = "state",
                       state = "51",
                       var = vars,
                       year = .x,
                       survey = "acs5") %>%
                       mutate(year = .x)
)

pop_all <- bind_rows(pop, pop_state)

pop_all$raceethn <- ifelse(pop_all$variable == "hispanic" | pop_all$variable == "nhwhite", "ethnicity", "race")

pop_alltotals <- pop_all %>%
  filter(variable != "hispanic" & variable != "nhwhite") %>%
  group_by(year, GEOID) %>%
  summarise(totalpop = sum(estimate))

pop_all <- merge(pop_all, pop_alltotals, by = c("GEOID", "year"))

# Save ----
write_csv(pop_all, file = "pop_race_ethn.csv")


