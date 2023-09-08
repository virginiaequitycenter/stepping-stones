# Showing the youth population racial and enthic demographics for the stepping stone supplemental report
# Contributor: Lee LeBoeuf

library(tidyverse)

# reading in data
popdat <- read.csv("pop_race_ethn.csv") %>%
  rename(Race = variable) %>%
  filter(year == 2021)

popdat$Race <- recode(popdat$Race, "aian" = "American Indian/Alaskan Native", 
                      "asian" = "Asian",
                      "black" = "Black",
                      "multi" = "Multiracial",
                      "nhpi" = "Native Hawaiian/Pacific Islander",
                      "other" = "Other",
                      "white" = "White",
                      "hispanic" = "Hispanic")

# dataframe for race
popdatrace <- popdat %>%
  filter(raceethn == 'race') %>%
  group_by(NAME) %>%
  mutate(totalyouthpop = sum(youthpop_count)) %>%
  ungroup()

popdatrace$youthperc <- round(popdatrace$youthpop_count / popdatrace$totalyouthpop * 100, 2)
popdatrace$allperc <- round(popdatrace$pop_count / popdatrace$totalpop * 100, 2)

# dataframe for ethnicity 
popdatethn <- popdat %>%
  group_by(NAME) %>%
  mutate(totalyouthpop = sum(youthpop_count)) %>%
  ungroup()

popdatethn <- popdatethn %>%
  group_by(raceethn, NAME) %>%
  mutate(groupcount = sum(youthpop_count)) %>%
  select(NAME, groupcount, totalyouthpop) %>%
  distinct()

popdatethn$Ethnicity <- ifelse(popdatethn$raceethn == "ethnicity", "Hispanic", "Non-Hispanic")

popdatethn$youthperc <- round(popdatethn$groupcount / popdatethn$totalyouthpop * 100, 2)

# plots -- stacked bars faceted by locality 

###### Race ######
# youth plot 
ggplot(popdatrace, aes(x = factor(1), y = youthperc, fill = Race)) +
  geom_bar(stat = "identity", width = 1) + theme_bw() + ggtitle("Racial breakdown of youth population in each locality in 2021") +
  ylab("Percent")  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") +
  facet_wrap(~NAME) 

# flipped axis
ggplot(popdatrace, aes(x = factor(1), y = youthperc, fill = Race)) +
  geom_bar(stat = "identity", width = 1) + theme_bw() + ggtitle("Racial breakdown of youth population in each locality in 2021") +
  xlab("Percent")  + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("") + coord_flip() +
  facet_wrap(~NAME, switch = T, nrow = 3)

# # everyone
# ggplot(popdatrace, aes(x = factor(1), y = allperc, fill = Race)) +
#   geom_bar(stat = "identity", width = 1) + theme_bw() + ggtitle("Racial breakdown of entire population in each locality in 2021") +
#   ylab("Percent")  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") +
#   facet_wrap(~NAME)

###### Ethnicity ######
# youth plot 
ggplot(popdatethn, aes(x = factor(1), y = youthperc, fill = Ethnicity)) +
  geom_bar(stat = "identity", width = 1) + theme_bw() + ggtitle("Ethnic breakdown of youth population in each locality in 2021") +
  ylab("Percent")  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") +
  facet_wrap(~NAME) 

# flipped axis
ggplot(popdatethn, aes(x = factor(1), y = youthperc, fill = Ethnicity)) +
  geom_bar(stat = "identity", width = 1) + theme_bw() + ggtitle("Ethnic breakdown of youth population in each locality in 2021") +
  xlab("Percent")  + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("") + coord_flip() +
  facet_wrap(~NAME, switch = T, nrow = 3) 

