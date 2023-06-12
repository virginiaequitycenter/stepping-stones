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
library(stringr)

# rename function 
rename <- function(data_names,originals,replacements){
  for(i in 1:length(originals)){
    data_names <- stringi::stri_replace_all_fixed(data_names, originals[i],replacements[i])
  }
  return(data_names)
}


# Download ----
years <- 2009:2021

#### Data for everyone

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

#### Data for population 17 and under 

# Pull variables, Charlottesville/Albemarle and VA----

# population variables 
varstable <- read.csv("popByRaceVariables.csv")

popunder18county <- map_dfr(years,
                      ~get_acs(
                        geography = "county",
                        state = "51",
                        county = c("003", "540"),
                        year = .x,
                        survey = "acs5",
                        var = varstable$OldName,
                        output = "tidy") %>%
                        mutate(year = .x))

popunder18state <- map_dfr(years,
                     ~get_acs(
                       geography = "state",
                       state = "51",
                       year = .x,
                       survey = "acs5",
                       var = varstable$OldName,
                       output = "tidy") %>%
                       mutate(year = .x))

# Prep data ----
# ..................................................
# Renaming variables 

popunder18county$variable <- rename(popunder18county$variable,varstable$OldName,varstable$NewName)
popunder18county[c("race", "GenderAge")] <- stringr::str_split_fixed(popunder18county$variable, "_", 2)

popunder18state$variable <- rename(popunder18state$variable,varstable$OldName,varstable$NewName)
popunder18state[c("race", "GenderAge")] <- stringr::str_split_fixed(popunder18state$variable, "_", 2)

# Adding columns of the same race and 
popunder18county <- popunder18county %>%
  group_by(race, year, GEOID) %>%
  summarise(estimate = sum(estimate))

popunder18state <- popunder18state %>%
  group_by(race, year, GEOID) %>%
  summarise(estimate = sum(estimate))

# binding states and localities together 
cpop <- rbind(popunder18county, popunder18state)

cpop <- cpop %>%
  dplyr::rename(youthpop_count = estimate,
                variable = race)

pop_all <- merge(cpop, pop_all, by = c("GEOID", "year", 'variable'))

pop_all <- pop_all %>%
  dplyr::rename(pop_count = estimate,
                pop_moe = moe)

# Save ----
write_csv(pop_all, file = "pop_race_ethn.csv")


