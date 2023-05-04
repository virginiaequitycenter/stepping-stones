# Data visualization for post-secondary education enrollment by race
# Author: Lee LeBoeuf 

# ..................................................
# Load Libraries, Read in Data, & Clean Variables ----
library(tidyverse)

dat <- read.csv("postsecondary_education_race.csv")

# ggplot can only have 6 unique shapes, and we have 9 racial categories
# I'm filtering out a couple for now
dat <- dat %>%
  filter(group != "Race Unknown") %>%
  filter(group != "Native Hawaiian")

# ..................................................
# Need to calculate the overall averages for each year to show in plot 

totals <- dat %>%
  group_by(year, locality) %>%
  summarise(cohortsize = sum(cohortsize, na.rm = T),
            enrolledany = sum(enrolledany, na.rm = T),
            enrolled4yr = sum(enrolled4yr, na.rm = T)) %>%
  mutate(percent_any = round(enrolledany / cohortsize * 100, 2),
         percent_4yr = round(enrolled4yr / cohortsize * 100, 2)) 

totals$group <- "Total"

plotdat <- rbind(dat, totals)

plotdat$group <- ifelse(plotdat$group == "2 or More", "Two or more", plotdat$group)

# Making NA rows for breaking up lines in ggplot 
# with the new way I code years later, 2010, 2012, 2014, and 2016 should all be NA years
yrs <- rep(c(2016.1, 2017.1, 2018.1, 2019.1, 2020.1, 2008.1, 2009.1, 2010.1, 2011.1, 2012.1, 2013.1, 2014.1, 2015.1), 3)
locality <- rep(c("Albemarle", "Charlottesville", "Virginia"), 13)
blanks <- data.frame(n = 1:39)
cols <- c("group", "cohortsize", "enrolledany", "enrolled4yr", "percent_any", "percent_4yr", "totalenrollperc", "lessthan")
blanks[cols] <- NA
blanks$year <- yrs
blanks$locality <- locality
blanks <- blanks %>%
  select(-n)

# Have to re-code years for the different localities separately and then re-bind the data frames together

plotdat$year <- ifelse(plotdat$locality == "Albemarle", plotdat$year - 0.33, 
                       ifelse(plotdat$locality == "Charlottesville", plotdat$year + 0.33, plotdat$year))
# ..................................................
# Creating plots

#### 4-year enrollment

# Filtering for racial groups whose group is large enough that we would expect at least one 
# student to enroll in 4-year college if total rate were equal across racial groups 
totalstime4yr <- data.frame(year = unique(plotdat$year), totalenrollperc = plotdat$percent_4yr[plotdat$group == "Total"])
plotdat4yr <- merge(plotdat, totalstime4yr, by = 'year')
plotdat4yr$lessthan <- ifelse(plotdat4yr$cohortsize < (100 / plotdat4yr$enrolled4yr), TRUE, FALSE)
plotdat4yr <- plotdat4yr %>%
  filter(lessthan == FALSE)

plotdat4yr <- plotdat4yr %>%
  filter(group != "Total")

plotdat4yr <- rbind(plotdat4yr, blanks)

ggplot() + geom_point(data = plotdat4yr, aes(year, percent_4yr, shape = group, color = locality),
                      position = 'identity', size = 3) + 
  geom_line(data = plotdat4yr,aes(x = year, y = percent_4yr, group = locality),
            color = 'gray') +
  xlab("Year") + ylab("Pecent of students")  + 
  ggtitle("") + scale_x_continuous(breaks = seq(2008, 2020, by = 1)) + 
  theme_bw()+ 
  scale_shape_discrete(na.translate = F) +
  labs(caption = "Note: Racial groups were only included for a given year and locality when the number of students enrolled 
  was high enough such that you would expect to see at least one student enrolled in a 4-year institution if the 
  overall rate were true for each racial group")+
  theme(plot.caption = element_text(hjust=0))


#### Any enrollment

# Filtering for racial groups whose group is large enough that we would expect at least one 
# student to enroll in 4-year college if total rate were equal across racial groups 
totalstimeAny <- data.frame(year = unique(plotdat$year), totalenrollperc = plotdat$percent_any[plotdat$group == "Total"])
plotdatAny <- merge(plotdat, totalstimeAny, by = 'year')
plotdatAny$lessthan <- ifelse(plotdatAny$cohortsize < (100 / plotdatAny$enrolled4yr), TRUE, FALSE)
plotdatAny <- plotdatAny %>%
  filter(lessthan == FALSE)

plotdatAny <- plotdatAny %>%
  filter(group != "Total")

plotdatAny <- rbind(plotdatAny, blanks)

ggplot() + geom_point(data = plotdatAny, aes(year, percent_4yr, shape = group, color = locality),
                      position = 'identity', size = 3) + 
  geom_line(data = plotdatAny,aes(x = year, y = percent_4yr, group = locality),
            color = 'gray') +
  xlab("Year") + ylab("Pecent of students")  + 
  ggtitle("") + scale_x_continuous(breaks = seq(2008, 2020, by = 1)) + 
  theme_bw()+ 
  scale_shape_discrete(na.translate = F) +
  labs(caption = "Note: Racial groups were only included for a given year and locality when the number of students enrolled 
  was high enough such that you would expect to see at least one student enrolled in a 4-year institution if the 
  overall rate were true for each racial group")+
  theme(plot.caption = element_text(hjust=0))
