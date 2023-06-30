# Creating data viz for stepping stones supplemental report by race 
# removing NHPI and AIAN for all plots because they represent such a small portion of the population 
# Contributor: Lee LeBoeuf

library(tidyverse)
library(stats)
library(plotrix)
library(stringr)


################# Children Living Below Poverty Threshold #################

# reading in data
povbyrace <- read.csv("child_pov_byRace.csv") %>%
  rename(Race = race,
         Locality = NAME) %>%
  filter(Race != "aian" & Race != "nhpi" & Race != "hispanic") %>% # all NA's for hispanic
  filter(GEOID != 51) # filtering out VA 

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
  filter(county_fips != 0) %>% # filtering out VA
  filter(year >= 2010) # only have data by race for 2010 and later 

povtot$Locality <- ifelse(povtot$county_fips == 3, "Albemarle",
                          ifelse(povtot$county_fips == 540, "Charlottesville", povtot$county_fips))

povtot$Race <- "Total"

# Plot 
ggplot() + geom_line(data = povbyrace, aes(year, pov_percent, color = Race) ) +
  geom_line(data = povtot, aes(year, pct_child_pov, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Percent of youth living in poverty by race") + 
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e")) + 
  facet_wrap(~ Locality)

################# Students Identified as Economically Disadvantaged #################

# reading in data
econbyrace <- read.csv("econ_disad_students_Race.csv") %>%
  filter(race != "American Indian or Alaska Native" & 
           race != "Unknown - Race/Ethnicity not provided" &
           race != "Native Hawaiian  or Pacific Islander") %>% 
  rename(Race = race) %>%
  filter(division_name != "Virginia") %>%
  filter(school_year != "2003-2004") # The total data doesn't include this year

econbyrace$Race <- recode(econbyrace$Race, 
                          "Black, not of Hispanic origin" = "Black",
                          "Non-Hispanic, two or more races" = "Multiracial",
                          "White, not of Hispanic origin" = "White")

econbyrace$division_name <- recode(econbyrace$division_name, 
                                   "Albemarle County" = "Albemarle",
                                   "Charlottesville City" = "Charlottesville")

# getting totals 
econtot <- read.csv("economically_disadvantaged_students.csv") %>%
  filter(division_name != "Virginia")

econtot$division_name <- recode(econtot$division_name, 
                                "Albemarle County" = "Albemarle",
                                "Charlottesville City" = "Charlottesville")
econtot$Race <- "Total"

# I couldn't figure out how to get the plot to work with the school years, so I had to 
# make a new year variable 
econtot$Year <- as.numeric(str_extract(econtot$school_year, "\\d+"))
econbyrace$Year <- as.numeric(str_extract(econbyrace$school_year, "\\d+"))

# Plot 
ggplot() + geom_line(data = econbyrace, aes(Year, percent, color = Race) ) +
  geom_line(data = econtot, aes(Year, percent, color = Race) )+ 
  ggtitle("Percent of economically disadvantaged students") +
  xlab("Year") + ylab("Percent") + theme_bw() + 
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e")) + 
  facet_wrap(~ division_name)


################# People Experiencing Homelessness #################
# reading in data 
hombyrace <- read.csv("pit_homelessness_race.csv")  %>%
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
  filter(Race == "Total")

# by race
hombyrace <- hombyrace %>%
  filter(Race != "Total")

# Plot 
ggplot() + geom_line(data = hombyrace, aes(year, rate, color = Race) ) +
  geom_line(data = homtotal, aes(year, rate, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Percent of people experiencing homelessness") +
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e", "#023047")) + 
  facet_wrap(~ Locality)
 

################# Low Birth-Weight Infants #################
# reading in data 
bw <- read.csv("low_birth_weight_race.csv") %>%
  rename(Race = group) %>%
  filter(locality != "Commonwealth Of Virginia")

bw$Race <- recode(bw$Race,
                  "black" = "Black",
                  "other" = "Other",
                  "white" = "White")
                  
bw$locality <- recode(bw$locality, 
                      "Albemarle County" = "Albemarle",
                      "Charlottesville City" = "Charlottesville")

# by race
bwrace <- bw %>%
  filter(Race != "all")

# totals 
bwtot <- bw %>%
  filter(Race == "all") 

# Plot 
ggplot() + geom_line(data = bwrace, aes(year, percent, color = Race) ) +
  geom_line(data = bwtot, aes(year, percent, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Percent of babies born with low weight") +
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e", "#023047")) + 
  facet_wrap(~ locality)

################# Infant Deaths #################
# reading in data 
id <- read.csv("infant_mortality_race.csv")%>%
  rename(Race = group) %>%
  filter(locality != "Commonwealth Of Virginia")

id$Race <- recode(id$Race,
                  "black" = "Black",
                  "other" = "Other",
                  "white" = "White")
                  
id$locality <- recode(id$locality, 
                      "Albemarle County" = "Albemarle",
                      "Charlottesville City" = "Charlottesville")

# by race
idrace <- id %>%
  filter(Race != "all")

# totals 
idtot <- id %>%
  filter(Race == "all") 

# Plot 
ggplot() + geom_line(data = idrace, aes(year, percent, color = Race) ) +
  geom_line(data = idtot, aes(year, percent, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Rate of infant mortality") +
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e", "#023047")) + 
  facet_wrap(~ locality)

# Charlottesville really does have 0 for Black and Other race, it's not missing 

################# Post-Secondary Enrollment #################
# reading in data 
psenroll <- read.csv("postsecondary_education_race.csv") %>%
  filter(group != "American Indian" & group != "Native Hawaiian") %>%
  rename(Race = group) %>%
  filter(locality != "Virginia")

psenroll$Race <- ifelse(psenroll$Race == "2 or More", "Multiracial", psenroll$Race)

psenrollrace <- psenroll %>%
  filter(Race != "All Students")

psenrolltot <- psenroll %>%
  filter(Race == "All Students")

# Plot 
ggplot() + geom_line(data = psenrollrace, aes(year, percent_any, color = Race) ) +
  geom_line(data = psenrolltot, aes(year, percent_any, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Rate of post-secondary enrollment") +
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e", "#023047")) + 
  facet_wrap(~ locality)

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

sustot <- albcville %>%
  filter(Race == 'Total')

susbyrace <- albcville %>%
  filter(Race != 'Total')

# Plot 
ggplot() + geom_line(data = susbyrace, aes(Year, meanpercsus, color = Race) ) +
  geom_line(data = sustot, aes(Year, meanpercsus, color = Race) )+ 
  xlab("Year") + ylab("Percent") + theme_bw() + 
  ggtitle("Suspension Rates") +
  scale_color_manual('Race', values = c('#457b9d', '#8cb369', '#f4e285', '#590d22', "#3c096c", "#ff595e", "#023047")) + 
  facet_wrap(~ Locality)
