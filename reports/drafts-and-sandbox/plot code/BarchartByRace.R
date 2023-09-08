# Creating data viz for stepping stones supplemental report by race 
# removing NHPI and AIAN for all plots because they represent such a small portion of the population 
# Contributor: Lee LeBoeuf

library(tidyverse)
library(stats)
library(plotrix)


################# Children Living Below Poverty Threshold #################

# reading in data
povbyrace <- read.csv("child_pov_byRace.csv") %>%
  rename(Race = race,
         Locality = NAME) %>%
  filter(Race != "aian" & Race != "nhpi" & Race != "hispanic") %>% # all NA's for hispanic
  filter(year == 2021)

povbyrace$Race <- recode(povbyrace$Race, 
                   "asian" = "Asian",
                   "black" = "Black",
                   "multi" = "Multiracial",
                   "other" = "Other",
                   "white" = "White", 
                   "hispanic" = "Hispanic")

povbyrace$Locality <- recode(povbyrace$Locality, 
                         "Albemarle County, Virginia" = "Albemarle",
                         "Charlottesville city, Virginia" = "Charlottesville")

# getting totals 
povtot <- read.csv("child_pov.csv") %>%
  filter(year == 2021)
povtot$Locality <- ifelse(povtot$county_fips == 0, "Virginia", 
                          ifelse(povtot$county_fips == 3, "Albemarle",
                                 ifelse(povtot$county_fips == 540, "Charlottesville", povtot$county_fips)))


# Plot 
ggplot() + geom_col(data = povbyrace, aes(y = pov_percent, x = Locality, fill = Locality)) + 
  geom_point(data = povtot, aes(y = pct_child_pov, x = Locality)) + ylab("Percent") + 
  ggtitle("Percent of youth living in poverty by race in 2021") + 
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 
# Charlottesville has 0 for Other race, it's not a missing value 

################# Students Identified as Economically Disadvantaged #################

# reading in data
econbyrace <- read.csv("econ_disad_students_Race.csv") %>%
  filter(race != "American Indian or Alaska Native" & 
           race != "Unknown - Race/Ethnicity not provided" &
           race != "Native Hawaiian  or Pacific Islander") %>% 
  rename(Race = race) %>%
  filter(school_year == '2022-2023')

econbyrace$Race <- recode(econbyrace$Race, 
                         "Black, not of Hispanic origin" = "Black",
                         "Non-Hispanic, two or more races" = "Multiracial",
                         "White, not of Hispanic origin" = "White")

econbyrace$division_name <- recode(econbyrace$division_name, 
                          "Albemarle County" = "Albemarle",
                          "Charlottesville City" = "Charlottesville")

# getting totals 
econtot <- read.csv("economically_disadvantaged_students.csv") %>%
  filter(school_year == '2022-2023') 

econtot$division_name <- recode(econtot$division_name, 
                                   "Albemarle County" = "Albemarle",
                                   "Charlottesville City" = "Charlottesville")

# Plot 
ggplot() + geom_col(data = econbyrace, aes(y = percent, x = division_name, fill = division_name)) + 
  geom_point(data = econtot, aes(y = percent, x = division_name)) + ylab("Percent") + xlab("Locality") +
  ggtitle("Percent of economically disadvantaged students in the 2022-2023 school year") +
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 


################# People Experiencing Homelessness #################
# reading in data 
hombyrace <- read.csv("pit_homelessness_race.csv")  %>%
  filter(year == 2022) %>%
  filter(group != "aian" & group != "nhpi") %>%
  rename(Race = group)

hombyrace$Race <- recode(hombyrace$Race, 
                          "black" = "Black",
                          "nonhispanic" = "Non-Hispanic",
                          "white" = "White",
                          "multi" = "Multiracial",
                          "total" = "Total",
                          "hispanic" = "Hispanic",
                          "asian" = "Asian") 

hombyrace <- pivot_longer(hombyrace, cols = c('rate_cvlalb', 'rate_region'),
                                  names_to = c('.value','Locality'),
                                  names_pattern = '(.+)_(cvlalb|region)')
# total
homtotal <- hombyrace %>%
  filter(Race == "Total") %>%
  rename(Group = Race)

# by race
hombyrace <- hombyrace %>%
  filter(Race != "Total")

# Plot 
ggplot() + geom_col(data = hombyrace, aes(y = rate, x = Locality, fill = Locality)) + 
  geom_point(data = homtotal, aes(y = rate, x = Locality)) + ylab("Percent") + xlab("Locality") +
  ggtitle("Percent of people experiencing homelessness in 2022") +
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d')) + 
  facet_wrap(~ Race) 

################# Low Birth-Weight Infants #################
# reading in data 
bw <- read.csv("low_birth_weight_race.csv") %>%
  filter(year == 2020) %>%
  rename(Race = group)

bw$Race <- recode(bw$Race,
                  "black" = "Black",
                  "other" = "Other",
                  "white" = "White")
                  
bw$locality <- recode(bw$locality, 
                      "Albemarle County" = "Albemarle",
                      "Charlottesville City" = "Charlottesville",
                      "Commonwealth Of Virginia" = "Virginia")

# by race
bwrace <- bw %>%
  filter(Race != "all")
                      
# totals 
bwtot <- bw %>%
  filter(Race == "all") %>%
  select(-Race)

# Plot 
ggplot() + geom_col(data = bwrace, aes(y = percent, x = locality, fill = locality)) + 
  geom_point(data = bwtot, aes(y = percent, x = locality)) + ylab("Percent") + xlab("Locality") +
  ggtitle("Percent of babies born with low weight in 2020") +
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 
# Charlottesville has 0 for other race, it's not a missing value 

################# Infant Deaths #################
# reading in data 
id <- read.csv("infant_mortality_race.csv")%>%
  filter(year == 2020) %>%
  rename(Race = group)

id$Race <- recode(id$Race,
                  "black" = "Black",
                  "other" = "Other",
                  "white" = "White")
                  
id$locality <- recode(id$locality, 
                      "Albemarle County" = "Albemarle",
                      "Charlottesville City" = "Charlottesville",
                      "Commonwealth Of Virginia" = "Virginia")

# by race
idrace <- id %>%
  filter(Race != "all")

# totals 
idtot <- id %>%
  filter(Race == "all") %>%
  select(-Race)

# Plot 
ggplot() + geom_col(data = idrace, aes(y = percent, x = locality, fill = locality)) + 
  geom_point(data = idtot, aes(y = percent, x = locality)) + ylab("Percent") + xlab("Locality") +
  ggtitle("Rate of infant mortality in 2020") +
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 
# Charlottesville really does have 0 for Black and Other race, it's not missing 

################# Post-Secondary Enrollment #################
# reading in data 
psenroll <- read.csv("postsecondary_education_race.csv") %>%
  filter(group != "American Indian" & group != "Native Hawaiian") %>%
  rename(Race = group) %>% 
  filter(year == 2020)

psenroll$Race <- ifelse(psenroll$Race == "2 or More", "Multiracial", psenroll$Race)

psenrollrace <- psenroll %>%
  filter(Race != "All Students")

psenrolltot <- psenroll %>%
  filter(Race == "All Students") %>%
  select(-Race)

# Plot 
ggplot() + geom_col(data = psenrollrace, aes(y = percent_any, x = locality, fill = locality)) + 
  geom_point(data = psenrolltot, aes(y = percent_any, x = locality)) + ylab("Percent") + xlab("Locality") +
  ggtitle("Percent youth enrolling in secondary education in 2020") +
  theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 
# Data are missing for Charlottesville Asian students 


################# Suspension data #################

# reading in data
nonprekVAall <- read.csv("school_suspension_CRDCnonprek.csv")

nonprekVAall$OSS_Total <- rowSums(nonprekVAall[,c('OSS_Black','OSS_Hispanic','OSS_White','OSS_TwoOrMore', 'OSS_Asian')], na.rm = T)

nonprekVAall$totalstudents <- nonprekVAall$num_Total

nonprekVAall <- nonprekVAall %>%
  filter(Year != 2009)

## Re-shaping them to long format 
k12long_count_oss <- pivot_longer(nonprekVAall, cols = c('num_Black', 'num_Hispanic', 'num_White',
                                                         'num_TwoOrMore', 'num_Asian', 'num_Total',
                                                         'OSS_Black', 'OSS_Hispanic', 'OSS_White',
                                                         'OSS_TwoOrMore','OSS_Asian', 'OSS_Total'),
                                  names_to = c('.value','Race'),
                                  names_pattern = '(.+)_(Black|White|Hispanic|Asian|TwoOrMore|Total)')

k12long_count_oss$Race <- ifelse(k12long_count_oss$Race == "TwoOrMore", "Two or more", k12long_count_oss$Race)

# Calculating the percent of students suspended in each racial group 
k12long_count_oss$percsus <- round(k12long_count_oss$OSS / k12long_count_oss$num * 100, 2)
# Calculating the percent of the students enrolled in each racial group 
k12long_count_oss$percenroll <- round(k12long_count_oss$num / k12long_count_oss$totalstudents * 100, 2)

allva <- k12long_count_oss %>%
  group_by(Year, Race) %>%
  summarise(meanpercsus = round(weighted.mean(percsus, num, na.rm = T), 2),
            se = std.error(percsus, na.rm = T),
            meanperenroll = round(mean(percenroll, na.rm = T), 2),
            meanenroll = mean(num))

# Filtering to just Charlottesville and Albemarle 

albcville <- k12long_count_oss %>%
  filter(LEA == "ALBEMARLE CO PBLC SCHS" | LEA == "CHARLOTTESVILLE CTY PBLC SCHS")

albcville$Locality <- ifelse(albcville$LEA == "ALBEMARLE CO PBLC SCHS", "Albemarle", "Charlottesville")

albcville <- albcville %>%
  group_by(Locality, Year, Race) %>%
  summarise(meanpercsus = round(weighted.mean(percsus, num, na.rm = T), 2),
            se = std.error(percsus, na.rm = T),
            meanperenroll = round(mean(percenroll, na.rm = T), 2),
            meanenroll = mean(num))

fulldat <- rbind(allva, albcville)
fulldat$Locality <- ifelse(is.na(fulldat$Locality) == T, "Virginia", fulldat$Locality)

mostrecentTot <- fulldat %>%
  filter(Year == 2017) %>%
  filter(Race == 'Total') %>%
  rename(group = Race)

mostrecent <- fulldat %>%
  filter(Year == 2017) %>%
  filter(Race != 'Total')

ggplot() + geom_col(data = mostrecent, aes(y = meanpercsus, x = Locality, fill = Locality)) + 
  geom_point(data = mostrecentTot, aes(y = meanpercsus, x = Locality)) + ylab("Percent") + 
  ggtitle("Percent of students suspended in the 2017-2018 school year") +
  theme_bw() + ylim(0, 15) + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
  facet_wrap(~ Race) 

################# Children in Foster Care #################
# reading in data 
# fosterbyrace <- read.csv("foster_care_race.csv") %>%
#   rename(Race = race) %>%
#   filter(Race != "aian" & Race != "nhpi" & Race != "unknown") %>%
#   filter(year == 2022)
# 
# fosterbyrace$Race <- recode(fosterbyrace$Race, 
#                          "asian" = "Asian",
#                          "black" = "Black",
#                          "multi" = "Multiracial",
#                          "other" = "Other",
#                          "white" = "White", 
#                          "hispanic" = "Hispanic")
# 
# # getting totals 
# fostertot <- read.csv("foster_care.csv") %>%
#   filter(year == 2022)
# 
# # Plot 
# ggplot() + geom_col(data = fosterbyrace, aes(y = percent, x = locality, fill = locality)) + 
#   geom_point(data = fostertot, aes(y = fc_rate, x = locality)) + ylab("Percent") + xlab("Locality") +
#   theme_bw() + scale_fill_manual('Locality', values = c('#8cb369', '#457b9d', "#3c096c")) + 
#   facet_wrap(~ Race) 
