# Stepping Stones Data: School Suspensions ----
# Updated 2023-03-03
# Contributor: Michele Claibourn, Charlie Bruce
# Acquire data from Safe Schools Information Resource (SSIR)
# https://p1pe.doe.virginia.gov/pti/selection.do
# Charlottesville, Albemarle, State

# Select report
# Choose all for everything except discipline type
#   in-school suspension, long-term suspsension, modified expulsion to suspension, short term suspension
# download disciplinary outcome csv file

library(tidyverse)
library(janitor)

sus_outcome <- read_csv("datadownloads/Disciplinary Outcome Report.csv", skip = 17) %>% 
  clean_names()

# clean up
sus_outcome <- sus_outcome %>% 
  filter(!(is.na(division_name))) %>% 
  select(division_name:individual_student_offenders) %>% 
  mutate(sus_count = as.numeric(individual_student_offenders),
         sus_count = ifelse(is.na(sus_count), 1, sus_count)) # make censored values 1

# charlottesville, albemarle ----
sus_cvlalb <- sus_outcome %>% 
  filter(division_number %in% c(2, 104))

# pivot to create columns by disciplinary type
sus_cvlalb_wide <- sus_cvlalb %>% 
  pivot_wider(id_cols = -individual_student_offenders, 
              names_from = discipline_type, values_from = sus_count) %>% 
  select(division_name:population,
         inschool = `IN-SCHOO L SUSPENSI ON`,
         longterm = `LONG-TER M SUSPENSI ON (OUT-OF-S CHOOL)`,
         modified = `MODIFIED EXPULSIO N TO SUSPENSI ON`,
         shortterm = `SHORT-TE RM SUSPENSI ON (OUT OF SCHOOL)`)

# create total and rate
sus_cvlalb_wide <- sus_cvlalb_wide %>% 
  mutate(total = inschool + longterm + modified + shortterm,
         rate = (total/population)*1000)

# state totals ----
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
         inschool = `IN-SCHOO L SUSPENSI ON`,
         longterm = `LONG-TER M SUSPENSI ON (OUT-OF-S CHOOL)`,
         modified = `MODIFIED EXPULSIO N TO SUSPENSI ON`,
         shortterm = `SHORT-TE RM SUSPENSI ON (OUT OF SCHOOL)`)

# create totals/rate
sus_state_wide <- sus_state_wide %>% 
  mutate(total = inschool + longterm + modified + shortterm,
         rate = (total/population)*1000)

# bind localities and state ----
suspensions <- bind_rows(sus_cvlalb_wide, sus_state_wide)

# have a peek
ggplot(suspensions, aes(x = school_year, y = rate, color = division_name, group = division_name)) +
  geom_line() +
  geom_point()

# save ----
write_csv(suspensions, "data/school_suspensions.csv")

# NOTES: consider redefining to short term suspensions only 
# and using running series from kidscount
# https://datacenter.kidscount.org/data/tables/9605-short-term-suspensions
# or combine with prior data -- though can't be sure what kinds of 
# suspensions the prior data represents (more than short term, definitely)