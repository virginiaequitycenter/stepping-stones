# Stepping Stones Data: School Suspensions ----
# Updated 2023-05-04
# Contributor: Michele Claibourn, Charlie Bruce
# Acquire data from Safe Schools Information Resource (SSIR)
# https://p1pe.doe.virginia.gov/pti/selection.do
# And KidsCount
# https://datacenter.kidscount.org/data/tables/9605-short-term-suspensions
# Charlottesville, Albemarle, State

# Select report ----
# Choose all for everything except discipline type
#   in-school suspension, long-term suspsension, modified expulsion to suspension, short term suspension
# download disciplinary outcome csv file

library(tidyverse)
library(janitor)
library(readxl)

# Read in SSIR data ----
sus_outcome <- read_csv("datadownloads/Disciplinary Outcome Report.csv", skip = 17) %>% 
  clean_names()

## clean up ----
# remove in-school suspension
sus_outcome <- sus_outcome %>% 
  filter(!(is.na(division_name)),
         discipline_type != "IN-SCHOO L SUSPENSI ON") %>% 
  select(division_name:individual_student_offenders) %>% 
  mutate(sus_count = as.numeric(individual_student_offenders),
         sus_count = ifelse(is.na(sus_count), 1, sus_count)) # make censored values 1

## charlottesville, albemarle ----
sus_cvlalb <- sus_outcome %>% 
  filter(division_number %in% c(2, 104)) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"),
         division_name = str_remove(division_name, " City| County"),
    division_name = str_replace(division_name, "Charlottes ville", "Charlottesville"))

# pivot to create columns by disciplinary type
sus_cvlalb_wide <- sus_cvlalb %>% 
  pivot_wider(id_cols = -individual_student_offenders, 
              names_from = discipline_type, values_from = sus_count) %>% 
  select(division_name:population,
         #inschool = `IN-SCHOO L SUSPENSI ON`,
         longterm = `LONG-TER M SUSPENSI ON (OUT-OF-S CHOOL)`,
         modified = `MODIFIED EXPULSIO N TO SUSPENSI ON`,
         shortterm = `SHORT-TE RM SUSPENSI ON (OUT OF SCHOOL)`)

# create totals, total rate, short term rate
sus_cvlalb_wide <- sus_cvlalb_wide %>% 
  mutate(total = longterm + modified + shortterm,
         totalrate = (total/population)*1000,
         shorttermrate = (shortterm/population)*1000)

## state totals ----
sus_state <- sus_outcome %>% 
  group_by(school_year, discipline_type) %>% 
  summarize(population = sum(population),
            sus_count = sum(sus_count)) %>% 
  mutate(division_number = 0,
         division_name = "Virginia")

# pivot
sus_state_wide <- sus_state %>% 
  pivot_wider(names_from = discipline_type, values_from = sus_count) %>% 
  select(school_year:division_name,
         #inschool = `IN-SCHOO L SUSPENSI ON`,
         longterm = `LONG-TER M SUSPENSI ON (OUT-OF-S CHOOL)`,
         modified = `MODIFIED EXPULSIO N TO SUSPENSI ON`,
         shortterm = `SHORT-TE RM SUSPENSI ON (OUT OF SCHOOL)`)

# create totals, total rate, short term rate
sus_state_wide <- sus_state_wide %>% 
  mutate(total = longterm + modified + shortterm,
         totalrate = (total/population)*1000,
         shorttermrate = (shortterm/population)*1000)

## bind localities and state ----
suspensions <- bind_rows(sus_cvlalb_wide, sus_state_wide) %>% 
  mutate(source = "VDOE SSIR")

# have a peek
ggplot(suspensions, aes(x = school_year, y = totalrate, color = division_name, group = division_name)) +
  geom_line() +
  geom_point()

ggplot(suspensions, aes(x = school_year, y = shorttermrate, color = division_name, group = division_name)) +
  geom_line() +
  geom_point()

# Bring in KidsCount ----
# short term suspension only
url <- "https://datacenter.aecf.org/rawdata.axd?ind=9605&loc=48"
dest <- "datadownloads/shortterm_suspensions.xlsx"
download.file(url, dest)

kcdata <- read_excel("datadownloads/shortterm_suspensions.xlsx", sheet = 1)

## clean up ----
# division_name, division_number, school_year, shortterm, shorttermrate, source
kc_sus <- kcdata %>% 
  filter(Location %in% c("Virginia", "Albemarle", "Charlottesville")) %>% 
  select(division_name = Location,
         school_year = TimeFrame,
         value_type = DataFormat,
         value = Data) %>% 
  mutate(school_year = str_remove(school_year, "AY "),
         school_year = str_remove(school_year, "(?<=-)[[:digit:]]{2}"),
         value = as.numeric(value)) %>% 
  pivot_wider(names_from = value_type, values_from = value) %>% 
  mutate(Percent = Percent*1000,
         source = "Kids Count",
         division) %>% 
  rename(shortterm = Number, shorttermrate = Percent)


# Combine ----
suspension_combined <- bind_rows(suspensions, kc_sus) %>% 
  mutate(division_number = case_when(
    division_name == "Albemarle" ~ 2,
    division_name == "Charlottesville" ~ 104,
    division_name == "Virginia" ~ 0
  ))

# have a peek
ggplot(suspension_combined, aes(x = school_year, y = shorttermrate,
                                color = division_name, group = division_name,
                                linetype = source)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = c(2,1))


# save ----
write_csv(suspension_combined, "data/school_suspensions.csv")
# suspensions <- read_csv("data/school_suspensions.csv")

suspensions %>% 
  filter(source == "VDOE SSIR" | 
           (source == "Kids Count" & school_year %in% c("2014-15", "2015-16"))) %>% 
ggplot(aes(x = school_year, y = shorttermrate,
                                color = division_name)) +
  geom_point() +
  geom_line(aes(group = division_name)) +
  scale_linetype_manual(values = c(2,1))
