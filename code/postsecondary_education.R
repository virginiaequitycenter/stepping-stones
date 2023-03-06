# Stepping Stones Data: Post-Secondary Education ----
# Updated 2023-03-03
# Contributor: Michele Claibourn
# Acquire data from VDOE Postsecondary Enrollment Reports
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Education, State Fiscal Stabilization Fund Indicator, "Postsecondary Enrollment Reports." 2008-2020.
# https://p1pe.doe.virginia.gov/postsec_public/

# For Charlottesville, Albemarle select 
#   FGI Cohort Year: 2008 through 2020 (individually)
#   Graduation Rate Type: Four Year Rate
#   School Division Results Selection: Charlottesville, Albemarle (individually)
#   Click View Excel
# For State select
#   FGI Cohort Year: 2008 through 2020 (individually)
#   Graduation Rate Type: Four Year Rate
#   State Results Selection
#   Click View Excel


# Libraries ----
library(tidyverse)
library(here)
library(readxl)
library(janitor)


# Read in files ----
cvlfiles <- list.files(path = "datadownloads/sfsf_cvl", pattern = "*.xlsx", full.names = TRUE)
albfiles <- list.files(path = "datadownloads/sfsf_alb", pattern = "*.xlsx", full.names = TRUE)
vafiles <- list.files(path = "datadownloads/sfsf_va", pattern = "*.xlsx", full.names = TRUE)

# ## try one
# yeartest <- read_excel(cvlfiles[1], sheet = 1, range = "A3") %>% 
#   names() %>% 
#   str_extract(pattern = "[0-9]{4}")
# 
# datatest <- read_excel(cvlfiles[1], sheet = 1, range = "A7:J18") 
# names(datatest) <- c("group", "cohortsize", "enrolledany", "percentany", 
#                      "enrolled4pub", "percent4pub", "enrolled4priv", "percent4priv",
#                      "enrolled2yr", "pecen2yr")

## read all
## charlottesville ----
years <- map_dfr(cvlfiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

cvl_data <- map(cvlfiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(cvl_data) <- years # add year as names for list

# rename columns to be identical across years
colnames = c("group", "cohortsize", "enrolledany", "percentany", 
             "enrolled4pub", "percent4pub", "enrolled4priv", "percent4priv",
             "enrolled2yr", "pecen2yr")

cvl_data <- map(cvl_data, ~ rename_with(., ~ colnames))

# bind into data frame
cvl <- bind_rows(cvl_data, .id = "year") %>% 
  mutate(locality = "Charlottesville")


## albemarle ----
years <- map_dfr(albfiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

alb_data <- map(albfiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(alb_data) <- years # add year as names for list

# rename columns to be identical across years
colnames = c("group", "cohortsize", "enrolledany", "percentany", 
             "enrolled4pub", "percent4pub", "enrolled4priv", "percent4priv",
             "enrolled2yr", "pecen2yr")

alb_data <- map(alb_data, ~ rename_with(., ~ colnames))

# bind into data frame
alb <- bind_rows(alb_data, .id = "year") %>% 
  mutate(locality = "Albemarle")


## virginia ----
years <- map_dfr(vafiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

va_data <- map(vafiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(va_data) <- years # add year as names for list

# rename columns to be identical across years
colnames = c("group", "cohortsize", "enrolledany", "percentany", 
             "enrolled4pub", "percent4pub", "enrolled4priv", "percent4priv",
             "enrolled2yr", "pecen2yr")

va_data <- map(va_data, ~ rename_with(., ~ colnames))

# bind into data frame
va <- bind_rows(va_data, .id = "year") %>% 
  mutate(locality = "Virginia")


# Bind localities ----
df_all <- bind_rows(cvl, alb, va)


# Prep data ----
df <- df_all %>% 
  filter(group == "All Students") %>% 
  mutate(across(-c("group", "locality"), as.numeric))


# have a peek
ggplot(df, aes(x = year, y = percentany, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(50,100))

# pivot to stack 2 year and 4 year
df_long <- df %>% 
  mutate(enrolled4yr = enrolled4pub + enrolled4priv) %>% 
  select(year, locality, cohortsize, enrolled4yr, enrolled2yr) %>% 
  pivot_longer(cols = starts_with("enrolled"), names_to = "type", values_to = "number") %>% 
  mutate(percent = (number/cohortsize)*100)

# have a peek
ggplot(df_long, aes(x = year, y = percent, fill = type)) + 
  geom_area() +
  facet_wrap(~locality)

# save data ----
write_csv(df, "data/postsecondary_education.csv")
