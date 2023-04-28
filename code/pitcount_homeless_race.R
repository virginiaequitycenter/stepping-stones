# Title: Stepping Stones Project
# Point-in-Time Count of People Experiencing Homelessness by race
# Michele Claibourn
# Last Updated: 2023-04-21

# Proposed Citation
# Blue Ridge Area Coalition for the Homeless, "Point-in-Time Count", 2022.

# ..................................................
# Load Libraries ----

library(tidyverse)
library(readxl)
library(janitor)
library(tidycensus)
# library(lubridate)


# ..................................................
# Read in BRACH data & prep ----
pit <- read_excel("datadownloads/PIT Count 2022 Homeless Populations.xls", 
                  sheet = "Totals",
                  range = "A30:K36")

pit <- pit %>% 
  select(race = 1, es = 3, ts = 5, sh = 7, un = 9, total = 11) %>% 
  mutate(race2 = case_when(
    race == "American Indian, Alaska Native, or Indigenous" ~ "aian",
    race == "Asian or Asian American" ~ "asian",
    race == "Black, African American, or African" ~ "black",
    race == "Native Hawaiian or Pacific Islander" ~ "nhpi",
    race == "White" ~ "white", 
    race == "Multiple Races" ~ "multi"),
    pit_percent = (total/sum(total))*100)
  


# ..................................................
# Read in populatio data & prep ----
region = c("540", "003", "065", "079", "109", "125")
county_pop <- get_acs(geography = "county",
                      state = "51",
                      county = region,
                      table = "B02001",
                      year = 2021,
                      survey = "acs5")

pop_total <- county_pop %>% 
  filter(variable == "B02001_001") %>% 
  summarize(total = sum(estimate))

pop <- county_pop %>% 
  filter(!(variable %in% c("B02001_001", "B02001_007", "B02001_009", "B02001_010"))) %>% 
  mutate(race2 = fct_recode(variable,
                             white = "B02001_002",
                             black = "B02001_003",
                             aian = "B02001_004",
                             asian = "B02001_005",
                             nhpi = "B02001_006",
                             multi = "B02001_008")
         ) %>% 
  group_by(race2) %>%
  summarize(pop = sum(estimate)) %>% 
  ungroup() %>% 
  mutate(pop_percent = (pop/pop_total$total)*100) 


# ..................................................
# Join and visualize ----
pit_pop <- pit %>% 
  left_join(pop) %>% 
  mutate(rdi = pit_percent/pop_percent)

# visualize percent: stacked bar
pit_pop %>% 
  select(race, race2, total, pop) %>% 
  pivot_longer(-c(race, race2), 
               names_to = "source", values_to = "count") %>% 
  ggplot(aes(x = source, y = count, fill = race2)) +
  geom_col(position = "fill")

# visualize percent: dots
pit_pop %>% 
  select(race, race2, pit_percent, pop_percent) %>% 
  pivot_longer(-c(race, race2), 
               names_to = "source", values_to = "percent") %>% 
  ggplot(aes(y = fct_reorder(race2, percent), x = percent)) +
  geom_line(aes(group = race2), color = "grey") +
  geom_point(aes(color = source))

# visualize ratio/index
pit_pop %>% 
  ggplot(aes(x = rdi, y = race2)) +
  geom_col() +
  geom_vline(xintercept = 1, color = "black") +
  scale_x_continuous(name = "Disproportionality Index", 
                     limits = c(-0.98, 9),
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3, 4, 5), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3", "4", "5"))


# combine aian and nhpi? or remove nhpi (it's zero)?

# ..................................................
# Export Data ----
write_csv(pit_pop, "data/pit_homelessness_race.csv")
# pit_pop <- read_csv("data/pit_homelessness_race.csv")
