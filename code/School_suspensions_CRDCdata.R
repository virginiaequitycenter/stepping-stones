

library(tidyverse)
library(stats)
library(ggpubr)
library(plotrix)

# reading in data
nonprekVAall <- read.csv("school_suspension_CRDCnonprek.csv")

nonprekVAall$OSS_Total <- rowSums(nonprekVAall[,c('OSS_Black','OSS_Hispanic','OSS_White','OSS_TwoOrMore', 'OSS_Asian')], na.rm = T)

nonprekVAall$totalstudents <- nonprekVAall$num_Total

nonprekVAall <- nonprekVAall %>%
  filter(Year != 2009)

## Re-shaping them to long format 
c <- pivot_longer(nonprekVAall, cols = c('num_Black', 'num_Hispanic', 'num_White',
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

# Making NA rows for breaking up lines in ggplot 
# with the new way I code years later, 2010, 2012, 2014, and 2016 should all be NA years
yrs <- c(2012, 2014, 2016)
blanks <- data.frame(n = 1:3)
cols <- c("Year", "Race", "meanpercsus", "se", "meanperenroll", "meanenroll", "totalsus", "lessthan")
blanks[cols] <- NA
blanks$Year <- yrs
blanks <- blanks %>%
  select(-n)

# Calculating means by race and year for plotting 
k12OSSmeans <- k12long_count_oss %>%
  group_by(Year, Race) %>%
  summarise(meanpercsus = round(weighted.mean(percsus, num, na.rm = T), 2),
            se = std.error(percsus, na.rm = T),
            meanperenroll = round(mean(percenroll, na.rm = T), 2),
            meanenroll = mean(num))

# Filtering for racial groups whose enrollment is large enough that we would expect at least one 
# suspension if rates were equitable across racial groups 
totalsusovertime <- data.frame(Year = unique(k12OSSmeans$Year), totalsus = k12OSSmeans$meanpercsus[k12OSSmeans$Race == "Total"])
k12OSSmeans <- merge(k12OSSmeans, totalsusovertime, by = 'Year')
k12OSSmeans$lessthan <- ifelse(k12OSSmeans$meanenroll < (100 / k12OSSmeans$totalsus), TRUE, FALSE)
k12OSSmeans <- k12OSSmeans %>%
  filter(lessthan == FALSE)

k12OSSmeans <- rbind(k12OSSmeans, blanks)

k12OSSmeans$Locality <- "All of Virginia"

# Charlottesville = CHARLOTTESVILLE CTY PBLC SCHS
# Albemarle = ALBEMARLE CO PBLC SCHS

albk12 <- k12long_count_oss %>%
  filter(LEA == "ALBEMARLE CO PBLC SCHS")

cvillek12 <- k12long_count_oss %>%
  filter(LEA == "CHARLOTTESVILLE CTY PBLC SCHS")

# Albemarle
# Calculating means by race and year for plotting 
albk12OSSmeans <- albk12 %>%
  group_by(Year, Race) %>%
  summarise(meanpercsus = round(weighted.mean(percsus, num, na.rm = T), 2),
            se = std.error(percsus, na.rm = T),
            meanperenroll = round(mean(percenroll, na.rm = T), 2),
            meanenroll = mean(num))

# Filtering for racial groups whose enrollment is large enough that we would expect at least one 
# suspension if rates were equitable across racial groups 
totalsusovertime <- data.frame(Year = unique(albk12OSSmeans$Year), totalsus = albk12OSSmeans$meanpercsus[albk12OSSmeans$Race == "Total"])
albk12OSSmeans <- merge(albk12OSSmeans, totalsusovertime, by = 'Year')
albk12OSSmeans$lessthan <- ifelse(albk12OSSmeans$meanenroll < (100 / albk12OSSmeans$totalsus), TRUE, FALSE)
albk12OSSmeans <- albk12OSSmeans %>%
  filter(lessthan == FALSE)

albk12OSSmeans <- rbind(albk12OSSmeans, blanks)

albk12OSSmeans$Locality <- "Albemarle"

# creating weird year variable for plotting
albk12OSSmeans$Year <- ifelse(albk12OSSmeans$Year == 2011, 2011.5,
                                     ifelse(albk12OSSmeans$Year == 2013, 2013.5,
                                            ifelse(albk12OSSmeans$Year == 2015, 2015.5,
                                                   ifelse(albk12OSSmeans$Year == 2017, 2017.5, albk12OSSmeans$Year))))

# Cville
cvillek12OSSmeans <- cvillek12 %>%
  group_by(Year, Race) %>%
  summarise(meanpercsus = round(weighted.mean(percsus, num, na.rm = T), 2),
            se = std.error(percsus, na.rm = T),
            meanperenroll = round(mean(percenroll, na.rm = T), 2),
            meanenroll = mean(num))

# Filtering for racial groups whose enrollment is large enough that we would expect at least one 
# suspension if rates were equitable across racial groups 
totalsusovertime <- data.frame(Year = unique(cvillek12OSSmeans$Year), totalsus = cvillek12OSSmeans$meanpercsus[cvillek12OSSmeans$Race == "Total"])
cvillek12OSSmeans <- merge(cvillek12OSSmeans, totalsusovertime, by = 'Year')
cvillek12OSSmeans$lessthan <- ifelse(cvillek12OSSmeans$meanenroll < (100 / cvillek12OSSmeans$totalsus), TRUE, FALSE)
cvillek12OSSmeans <- cvillek12OSSmeans %>%
  filter(lessthan == FALSE)

cvillek12OSSmeans <- rbind(cvillek12OSSmeans, blanks)

cvillek12OSSmeans$Locality <- "Charlottesville"

# creating weird year variable for plotting
cvillek12OSSmeans$Year <- ifelse(cvillek12OSSmeans$Year == 2011, 2010.5,
                                     ifelse(cvillek12OSSmeans$Year == 2013, 2012.5,
                                            ifelse(cvillek12OSSmeans$Year == 2015, 2014.5,
                                                   ifelse(cvillek12OSSmeans$Year == 2017, 2016.5, cvillek12OSSmeans$Year))))

dat <- rbind(k12OSSmeans, albk12OSSmeans, cvillek12OSSmeans)

### Plots 
# all levels together 
ggplot() + geom_point(data = dat, aes(Year, meanpercsus, shape = Locality, color = Race),
                      position = 'identity', size = 3) + 
  geom_line(data = dat,aes(x = Year, y = meanpercsus, group = Locality),
            color = 'gray') +
  xlab("Year") + ylab("Average % of students receiving OSS")  + 
  ggtitle("") + scale_x_continuous(breaks = seq(2011, 2017, by = 2)) + 
  theme_bw()+ 
  scale_colour_discrete(na.translate = F) +
  labs(caption = "Note: Racial groups were only included for a given year and locality when the average
       number of students enrolled was high enough such that you would expect to see at least one suspension
       if the average overall rate were true for each racial group")+
  theme(plot.caption = element_text(hjust=0))

