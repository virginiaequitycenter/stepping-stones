# Title: Stepping Stones Project
# Housing-Renters Paying More than 30% of Income for Housing (%) 
# Team 6: Connor Eads, Maria Morrissey, Vani Agarwal, Melinda Wong
# Last Updated: 2023-02-15

# Proposed Citation
# American Community Survey table B25070 "Gross Rent as a Percentage of Household Income in the Past 12 Months", 2009-2021. 
# Accessed through tidycensus, documentation available at https://www.socialexplorer.com/data/ACS2016/metadata/?ds=ACS16&table=B25070
# Accessed 2023-02-15

# ..................................................
# Load Libraries, Read in Data, & Clean Variables ----

# Load libraries
library(tidyverse)
library(janitor)
library(tidycensus)

#Data point ---- 
detail_vars <- load_variables(2021, "acs5", cache = TRUE)

vars <- c(total = "B25070_001", 
          thirty = "B25070_007", 
          thirty_five = "B25070_008", 
          forty = "B25070_009",
          fifty = "B25070_010") 

years <- 2009:2021

rent <- map_dfr(years,
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

##create a sum of the number of rents 
rent_total <- rent %>% 
  filter (variable == "total") %>% 
  select(GEOID, NAME, year, total = estimate)

##the number of people that are paying rent above 30% of their income 
rent_share <- rent %>% 
  filter (variable != "total")%>% 
  group_by (GEOID, NAME, year)%>% 
  summarize(rent_over_30 = sum(estimate))

## joining together dataframes
loc_rent_over_30 <- left_join(rent_share, rent_total) %>% 
  mutate(percent_over_30 = rent_over_30/total)

### STATE DATA
rent_state <- map_dfr(years,
                ~get_acs(
                  geography = "state",
                  state = "51",
                  year = .x,
                  survey = "acs5",
                  var = vars,
                  output = "tidy") %>%
                  mutate(year = .x)
)

##create a sum of the number of rents 
state_rent_total <- rent_state %>% 
  filter (variable == "total") %>% 
  select(GEOID, NAME, year, total = estimate)

##the number of people that are paying rent above 30% of their income 
state_rent_share <- rent_state %>% 
  filter (variable != "total")%>% 
  group_by (GEOID, NAME, year)%>% 
  summarize(rent_over_30 = sum(estimate))

## joining together dataframes
state_rent_over_30 <- left_join(state_rent_share, state_rent_total) %>% 
  mutate(percent_over_30 = rent_over_30/total)

## Combining county and state data
rent_over_30 <- bind_rows(loc_rent_over_30, state_rent_over_30)

# have a peek
ggplot(rent_over_30, aes(x = year, y = percent_over_30, color = NAME)) + geom_line()


# ..................................................
# Export Data ----
write_csv(rent_over_30, "data/rent_burdened.csv")

  