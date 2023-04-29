# Stepping Stones Data: Prenatal Care ----
# Updated 2023-04-27
# Contributors: Mary Katherine West, Michele Claibourn
# Prepare requested data
# Charlottesville, Albemarle, Virginia

# These data were obtained via a data request submitted at
# https://www.vdh.virginia.gov/disease-prevention/data-request-form/
# available at: https://www.vdh.virginia.gov/disease-prevention/disease-prevention/hiv-aids-sexually-transmitted-disease-std-hepatitis-reports/

# Description of Data Request
# 1. What condition: HIV, gonorrhea, chlamydia, syphilis
# 2. What area do you need data for: State of Virginia, Albemarle County, City of Charlottesville
# 3. What time frame is the data to cover? 2001 to 2022
# 4. Provide a detailed description of the data you are requesting: The annual incidence of 
# HIV, gonorrhea, chlamydia, and syphilis for populations 19 and under in the
# State of Virginia, the City of Charlottesville, and Albemarle County (separately) 
# 5. What format would you like the data? Table, csv file
# 6. Describe the purpose of this data and how you plan to use it: The UVA Equity Center is 
# partnering with the City of Charlottesville to update the Stepping Stones Report, 
# a collection of metrics that gauge wellbeing of children and families in the 
# Charlottesville/Albemarle area. Sexually transmitted diseases in youth
# populations is one of these metrics. The data requested will be used within this report
# which is sponsored by the City's Department of Human Services.
# 7. Please share any additional comments or information regarding your data request: The current iteration of the report we are working to update ca be found at the following link:
# https://www.charlottesville.gov/DocumentCenter/View/8995/Stepping-Stones-2019-Final?bidId=
# The Youth Health: Sexually Transmitted diseases metric can be found on page 33.

# Proposed citation
# Virginia Department of Health, Division of Disease Prevention, "STD, HIV/AIDS Data Reports." 2001-2022

library(tidyverse)
library(readxl)
library(zoo)

# STIs ----
sti1 <- read_excel("datadownloads/vdhrequest/Youth STIs (S,CT, GC).xlsx",
                   sheet = "Syphilis", range = "a2:t5")

names(sti1) <- c("locality", paste0("syp_", seq(2001,2019,1), "-", seq(2003,2021,1)))

sti2 <- read_excel("datadownloads/vdhrequest/Youth STIs (S,CT, GC).xlsx",
                   sheet = "CT", range = "a2:t5")

names(sti2) <- c("locality", paste0("ct_", seq(2001,2019,1), "-", seq(2003,2021,1)))

sti3 <- read_excel("datadownloads/vdhrequest/Youth STIs (S,CT, GC).xlsx",
                   sheet = "GC", range = "a2:t5")

names(sti3) <- c("locality", paste0("gc_", seq(2001,2019,1), "-", seq(2003,2021,1)))

## reshape
sti1_long <- sti1 %>% 
  pivot_longer(-locality, names_to = "year", names_prefix = "syp_", values_to = "inc_rate") %>% 
  mutate(type = "syphilis")

sti2_long <- sti2 %>% 
  pivot_longer(-locality, names_to = "year", names_prefix = "ct_", values_to = "inc_rate") %>% 
  mutate(type = "chlamydia")

sti3_long <- sti3 %>% 
  pivot_longer(-locality, names_to = "year", names_prefix = "gc_", values_to = "inc_rate") %>% 
  mutate(type = "gonorrhea")

## combine
stis <- bind_rows(sti1_long, sti2_long, sti3_long)

# have a peek
ggplot(stis, aes(x = year, y = inc_rate, color = type)) +
  geom_line(aes(group = type)) +
  geom_point() +
  facet_wrap(~locality)


stis_total <- stis %>% 
  group_by(locality, year) %>% 
  summarize(incrate_stis = sum(inc_rate)) %>% 
  mutate(locality = str_remove(locality, "City of "),
         year3 = year,
         year = str_sub(as.character(year3), 1,4),
         year = as.numeric(year) + 1)

# HIV ----
va_hiv <- read_excel("datadownloads/vdhrequest/HIV_Rate_updated (1).xlsx",
                     sheet = 1, range = "a4:b26") %>% 
  mutate(locality = "Virginia") %>% 
  rename(year = `Diagnosis Year`, incrate = `HIV Rate per 1,000`)
cvl_hiv <- read_excel("datadownloads/vdhrequest/HIV_Rate_updated (1).xlsx",
                      sheet = 1, range = "d4:e26") %>% 
  mutate(locality = "Charlottesville") %>% 
  rename(year = `Diagnosis Year`, incrate = `HIV Rate per 1,000`)
alb_hiv <- read_excel("datadownloads/vdhrequest/HIV_Rate_updated (1).xlsx",
                      sheet = 1, range = "g4:h26") %>% 
  mutate(locality = "Albemarle") %>% 
  rename(year = `Diagnosis Year`, incrate = `HIV Rate per 1,000`)

## combine
hiv <- bind_rows(cvl_hiv, alb_hiv, va_hiv)

## generate 3 year rolling average
hiv_3yr <- hiv %>% 
  arrange(locality, year) %>% 
  group_by(locality) %>% 
  mutate(incrate_hiv = zoo::rollmean(incrate, k = 3, fill = NA, align = "center")) %>% 
  ungroup()


# Combine STI and HIV ----
sti_hiv <- stis_total %>% 
  left_join(hiv_3yr %>% select(year, locality, incrate_hiv)) %>% 
  mutate(total_incrate_3yr = incrate_stis + incrate_hiv)


## have a peek
ggplot(sti_hiv, aes(x = year, y = total_incrate_3yr, color = locality)) +
  geom_line() +
  geom_point()

ggplot(sti_hiv, aes(x = year, y = incrate_stis, color = locality)) +
  geom_line() +
  geom_point()

# Save ----
write_csv(sti_hiv, "data/youth_sti.csv")
