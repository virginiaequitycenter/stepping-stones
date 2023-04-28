# Stepping Stones Data: Child population/race ----
# Updated 2023-04-23
# Contributor: Michele Claibourn
# Generate locality and state child population estimates by race
#   used to create percents or rates for multiple metrics
# Using American Community Survey, 5-year estimates
# Table B01001
# Charlottesville, Albemarle, State

# Proposed Citation
# American Community Survey table B01001A-I "Sex by Age, by Race", 2009-2021. 
# Accessed through tidycensus, documentation available at https://www.socialexplorer.com/data/ACS2016/metadata/?ds=ACS16&table=B25070
# Accessed 2023-04-23

# Libraries ----
library(tidyverse)
library(janitor)
library(tidycensus)


# Download ----
years <- 2009:2021

## white ----
vars <- c(popm_u5 = "B01001A_003", 
          popm_5to9 = "B01001A_004", 
          popm_10to14 = "B01001A_005",
          popm_15to17 = "B01001A_006",
          popf_u5 = "B01001A_018", 
          popf_5to9 = "B01001A_019", 
          popf_10to14 = "B01001A_020",
          popf_15to17 = "B01001A_021")

pop_child_white <- map_dfr(years,
                           ~get_acs(
                             geography = "county",
                             state = "51",
                             county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_white <- pop_child_white %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "white")

# repeat for state
pop_child_white_state <- map_dfr(years,
                           ~get_acs(
                             geography = "state",
                             state = "51",
                             #county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_white_state <- pop_child_white_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "white")

pop_child_white <- bind_rows(pop_child_white, pop_child_white_state)

## black ----
vars <- c(popm_u5 = "B01001B_003", 
          popm_5to9 = "B01001B_004", 
          popm_10to14 = "B01001B_005",
          popm_15to17 = "B01001B_006",
          popf_u5 = "B01001B_018", 
          popf_5to9 = "B01001B_019", 
          popf_10to14 = "B01001B_020",
          popf_15to17 = "B01001B_021")

pop_child_black <- map_dfr(years,
                           ~get_acs(
                             geography = "county",
                             state = "51",
                             county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_black <- pop_child_black %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "black")

# repeat for state
pop_child_black_state <- map_dfr(years,
                                 ~get_acs(
                                   geography = "state",
                                   state = "51",
                                   #county = c("003", "540"),
                                   var = vars,
                                   year = .x,
                                   survey = "acs5") %>%
                                   mutate(year = .x)
)

pop_child_black_state <- pop_child_black_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "black")

pop_child_black <- bind_rows(pop_child_black, pop_child_black_state)

## ai/an ----
vars <- c(popm_u5 = "B01001C_003", 
          popm_5to9 = "B01001C_004", 
          popm_10to14 = "B01001C_005",
          popm_15to17 = "B01001C_006",
          popf_u5 = "B01001C_018", 
          popf_5to9 = "B01001C_019", 
          popf_10to14 = "B01001C_020",
          popf_15to17 = "B01001C_021")

pop_child_aian <- map_dfr(years,
                          ~get_acs(
                            geography = "county",
                            state = "51",
                            county = c("003", "540"),
                            var = vars,
                            year = .x,
                            survey = "acs5") %>%
                            mutate(year = .x)
)

pop_child_aian <- pop_child_aian %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "aian")

# repeat for state
pop_child_aian_state <- map_dfr(years,
                                 ~get_acs(
                                   geography = "state",
                                   state = "51",
                                   #county = c("003", "540"),
                                   var = vars,
                                   year = .x,
                                   survey = "acs5") %>%
                                   mutate(year = .x)
)

pop_child_aian_state <- pop_child_aian_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "aian")

pop_child_aian <- bind_rows(pop_child_aian, pop_child_aian_state)


## asian ----
vars <- c(popm_u5 = "B01001D_003", 
          popm_5to9 = "B01001D_004", 
          popm_10to14 = "B01001D_005",
          popm_15to17 = "B01001D_006",
          popf_u5 = "B01001D_018", 
          popf_5to9 = "B01001D_019", 
          popf_10to14 = "B01001D_020",
          popf_15to17 = "B01001D_021")

pop_child_asian <- map_dfr(years,
                           ~get_acs(
                             geography = "county",
                             state = "51",
                             county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_asian <- pop_child_asian %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "asian")

# repeat for state
pop_child_asian_state <- map_dfr(years,
                                ~get_acs(
                                  geography = "state",
                                  state = "51",
                                  #county = c("003", "540"),
                                  var = vars,
                                  year = .x,
                                  survey = "acs5") %>%
                                  mutate(year = .x)
)

pop_child_asian_state <- pop_child_asian_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "asian")

pop_child_asian <- bind_rows(pop_child_asian, pop_child_asian_state)

## nhpi ----
vars <- c(popm_u5 = "B01001E_003", 
          popm_5to9 = "B01001E_004", 
          popm_10to14 = "B01001E_005",
          popm_15to17 = "B01001E_006",
          popf_u5 = "B01001E_018", 
          popf_5to9 = "B01001E_019", 
          popf_10to14 = "B01001E_020",
          popf_15to17 = "B01001E_021")

pop_child_nhpi <- map_dfr(years,
                          ~get_acs(
                            geography = "county",
                            state = "51",
                            county = c("003", "540"),
                            var = vars,
                            year = .x,
                            survey = "acs5") %>%
                            mutate(year = .x)
)

pop_child_nhpi <- pop_child_nhpi %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "nhpi")

# repeat for state
pop_child_nhpi_state <- map_dfr(years,
                                 ~get_acs(
                                   geography = "state",
                                   state = "51",
                                   #county = c("003", "540"),
                                   var = vars,
                                   year = .x,
                                   survey = "acs5") %>%
                                   mutate(year = .x)
)

pop_child_nhpi_state <- pop_child_nhpi_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "nhpi")

pop_child_nhpi <- bind_rows(pop_child_nhpi, pop_child_nhpi_state)

## other ----
vars <- c(popm_u5 = "B01001F_003", 
          popm_5to9 = "B01001F_004", 
          popm_10to14 = "B01001F_005",
          popm_15to17 = "B01001F_006",
          popf_u5 = "B01001F_018", 
          popf_5to9 = "B01001F_019", 
          popf_10to14 = "B01001F_020",
          popf_15to17 = "B01001F_021")

pop_child_other <- map_dfr(years,
                           ~get_acs(
                             geography = "county",
                             state = "51",
                             county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_other <- pop_child_other %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "other")

# repeat for state
pop_child_other_state <- map_dfr(years,
                                ~get_acs(
                                  geography = "state",
                                  state = "51",
                                  #county = c("003", "540"),
                                  var = vars,
                                  year = .x,
                                  survey = "acs5") %>%
                                  mutate(year = .x)
)

pop_child_other_state <- pop_child_other_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "other")

pop_child_other <- bind_rows(pop_child_other, pop_child_other_state)

## multi ----
vars <- c(popm_u5 = "B01001G_003", 
          popm_5to9 = "B01001G_004", 
          popm_10to14 = "B01001G_005",
          popm_15to17 = "B01001G_006",
          popf_u5 = "B01001G_018", 
          popf_5to9 = "B01001G_019", 
          popf_10to14 = "B01001G_020",
          popf_15to17 = "B01001G_021")

pop_child_multi <- map_dfr(years,
                           ~get_acs(
                             geography = "county",
                             state = "51",
                             county = c("003", "540"),
                             var = vars,
                             year = .x,
                             survey = "acs5") %>%
                             mutate(year = .x)
)

pop_child_multi <- pop_child_multi %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "multi")

# repeat for state
pop_child_multi_state <- map_dfr(years,
                                 ~get_acs(
                                   geography = "state",
                                   state = "51",
                                   #county = c("003", "540"),
                                   var = vars,
                                   year = .x,
                                   survey = "acs5") %>%
                                   mutate(year = .x)
)

pop_child_multi_state <- pop_child_multi_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(race = "multi")

pop_child_multi <- bind_rows(pop_child_multi, pop_child_multi_state)

## nh-white ----
vars <- c(popm_u5 = "B01001H_003", 
          popm_5to9 = "B01001H_004", 
          popm_10to14 = "B01001H_005",
          popm_15to17 = "B01001H_006",
          popf_u5 = "B01001H_018", 
          popf_5to9 = "B01001H_019", 
          popf_10to14 = "B01001H_020",
          popf_15to17 = "B01001H_021")

pop_child_nhwhite <- map_dfr(years,
                             ~get_acs(
                               geography = "county",
                               state = "51",
                               county = c("003", "540"),
                               var = vars,
                               year = .x,
                               survey = "acs5") %>%
                               mutate(year = .x)
)

pop_child_nhwhite <- pop_child_nhwhite %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(ethn = "nhwhite")

# repeat for state
pop_child_nhwhite_state <- map_dfr(years,
                                 ~get_acs(
                                   geography = "state",
                                   state = "51",
                                   #county = c("003", "540"),
                                   var = vars,
                                   year = .x,
                                   survey = "acs5") %>%
                                   mutate(year = .x)
)

pop_child_nhwhite_state <- pop_child_nhwhite_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(ethn = "nhwhite")

pop_child_nhwhite <- bind_rows(pop_child_nhwhite, pop_child_nhwhite_state)

## hisp ----
vars <- c(popm_u5 = "B01001I_003", 
          popm_5to9 = "B01001I_004", 
          popm_10to14 = "B01001I_005",
          popm_15to17 = "B01001I_006",
          popf_u5 = "B01001I_018", 
          popf_5to9 = "B01001I_019", 
          popf_10to14 = "B01001I_020",
          popf_15to17 = "B01001I_021")

pop_child_hisp <- map_dfr(years,
                          ~get_acs(
                            geography = "county",
                            state = "51",
                            county = c("003", "540"),
                            var = vars,
                            year = .x,
                            survey = "acs5") %>%
                            mutate(year = .x)
)

pop_child_hisp <- pop_child_hisp %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(ethn = "hisp")

# repeat for state
pop_child_hisp_state <- map_dfr(years,
                                   ~get_acs(
                                     geography = "state",
                                     state = "51",
                                     #county = c("003", "540"),
                                     var = vars,
                                     year = .x,
                                     survey = "acs5") %>%
                                     mutate(year = .x)
)

pop_child_hisp_state <- pop_child_hisp_state %>% 
  group_by(GEOID, NAME, year) %>% 
  summarize(pop_count = sum(estimate)) %>% 
  mutate(ethn = "hisp")

pop_child_hisp <- bind_rows(pop_child_hisp, pop_child_hisp_state)


# Combine child_pop_race ----
pop_child_race <- bind_rows(pop_child_white, pop_child_black,
                            pop_child_aian, pop_child_asian,
                            pop_child_nhpi, pop_child_other,
                            pop_child_multi) 

pop_child_race <- pop_child_race %>% 
  group_by(GEOID, NAME, year) %>% 
  mutate(total_count = sum(pop_count)) %>% 
  ungroup() %>% 
  mutate(pop_percent = pop_count/total_count,
         group = "race")

# combine child_pop_ethn
pop_child_ethn <- bind_rows(pop_child_nhwhite, pop_child_hisp)

pop_child_ethn <- pop_child_ethn %>% 
  left_join(pop_child_race %>% 
              select(GEOID, NAME, year, total_count) %>% 
              distinct()  
  ) %>% 
  mutate(pop_percent = pop_count/total_count,
         group = "ethn")

# Combine race and ethnicity ----
# (will need to filter by group - race, ethn)
pop_child <- bind_rows(pop_child_race, pop_child_ethn)


# Save ----
write_csv(pop_child, file = "data/child_pop_race_ethn.csv")
