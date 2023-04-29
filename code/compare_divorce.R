# compare divorce data

library(tidyverse)

divorce_vdh <- read_csv("data/divorce_vdh.csv")
divorce_cc <- read_csv("data/divorce_circuit.csv")

# compare entire series
ggplot(divorce_vdh, 
                  aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,6)) +
  scale_x_continuous(breaks = seq(2000,2022, 2)) 

ggplot(divorce_cc, 
                  aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,6)) +
  scale_x_continuous(breaks = seq(2000,2022, 2)) 

# together
divorce_vdh <- divorce_vdh %>% 
  mutate(locality = str_remove(locality, " County| City"))

combined <- bind_rows(
  divorce_cc %>% mutate(source = "circuit court"),
  divorce_vdh %>% mutate(source = "health dept")
)

## facet
ggplot(combined, aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  geom_point() +
  facet_wrap(~source)

## on same graph
ggplot(combined, aes(x = year, y = divorce_rate, 
                     color = locality, linetype = source)) +
  geom_line()





