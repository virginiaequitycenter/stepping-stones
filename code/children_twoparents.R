# Stepping Stones Data: Children living with 2 parents ----
# Family characteristics-Children living with two parents (%) 
# Updated 2023-02-09
# Authors: Jayla Hart, Somin Lee, Charlie Bruce


# Citation: U.S. Census, American Community Survey 5-year estimates, 2010-2021
# "AGE AND NATIVITY OF OWN CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY NUMBER AND NATIVITY OF PARENTS"
# Table B05009. Retrieved February 16, 2023


# Load libraries ----
library(tidyverse)
#library(readxl)
library(janitor)
library(tidycensus)

# To use the tidycensus package, create a census api key
#   http://api.census.gov/data/key_signup.html

# Then you need to set your census api key: 
# census_api_key("YOUR API KEY GOES HERE", install = TRUE) 

# Find variable ids ----
# subject_vars <- load_variables(2021, "acs5", cache = TRUE)

# B05009_003 = Children under 6 years living with two parents
# B05009_021 = Children 6 - 17 years living with two parents
# B05009_001 = Total Children in area

acs_vars <- c(todlerhh = "B05009_003",
                   childhh = "B05009_021",
                   total = "B05009_001")
                   
# Retrieve Locality data  ----
# test
hh_2021 <- get_acs(geography = "county", # County observation
                            state = "51", # Virginia
                            county = c("003", "540"), # Cville/Alb
                            year = 2021, 
                            survey = "acs5", # The survey
                            var = acs_vars, #
                            output = "tidy",)

years <- 2010:2021 

hh_albcvl <- map_dfr(years, # map dataframe rowbind(across list of years)
                       ~get_acs(
                         geography = "county",
                         state = "51",
                         county = c("003", "540"),
                         year = .x, # .x = represents elements in list
                         survey = "acs5",
                         var = acs_vars,
                         output = "wide") %>%
                         mutate(year = .x)
)

# rename variables
hh_albcvl <- rename(hh_albcvl, 
                       todlermoe = todlerhhM,
                       todlerestimate = todlerhhE,
                       childestimate = childhhE,
                       childmoe = childhhM,
                       total = totalE,
                       totalmoe = totalM)

# sum children living with 2 parents
hh_albcvl <- hh_albcvl %>% 
  mutate(sum = (todlerestimate + childestimate),
         percent = (sum/total))


# Retrive state wide data ----
hh_statewide <- map_dfr(years,
                    ~get_acs(
                      geography = "state",
                      state = "51",
                      year = .x,
                      var = acs_vars,
                      output = "wide") %>%
                      mutate(year = .x)
) # same function as before except justwith state fips

# rename variables
hh_statewide <- rename(hh_statewide, 
                       todlermoe = todlerhhM,
                       todlerestimate = todlerhhE,
                       childestimate = childhhE,
                       childmoe = childhhM,
                      total = totalE,
                      totalmoe = totalM)

# sum children living with 2 parents
hh_statewide <- hh_statewide %>% 
  mutate(sum = (todlerestimate + childestimate),
         percent = (sum/total))

# Bind locality and state data ----
hh_bind <- bind_rows(hh_albcvl, hh_statewide)

# have a peek
ggplot(hh_bind, aes(x = year, y = percent, color = NAME)) + geom_line()


# Save data ----
write_csv(hh_bind, "data/children_twoparents.csv")
