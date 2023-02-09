# compare divorce data

library(tidyverse)
library(patchwork)

divorce_vdh <- read_csv("data/divorce_vdh.csv")
divorce_cc <- read_csv("data/divorce_circuit.csv")

# compare entire series
vdhplot <- ggplot(divorce_vdh, 
                  aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,4.5)) +
  scale_x_continuous(breaks = seq(2000,2022, 2)) 

ccplot <- ggplot(divorce_cc, 
                  aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,4.5)) +
  scale_x_continuous(breaks = seq(2000,2022, 2)) 

ccplot + vdhplot + plot_layout(guides='collect')

# compare years of overlap

vdhplot <- ggplot(divorce_vdh %>% filter(year > 2012 & year < 2019), 
                  aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,4.5)) +
  scale_x_continuous(breaks = seq(2013,2018,1)) 

ccplot <- ggplot(divorce_cc %>% filter(year > 2012 & year < 2019), 
                 aes(x = year, y = divorce_rate, color = locality)) +
  geom_line() +
  scale_y_continuous(limits = c(0,4.5)) +
  scale_x_continuous(breaks = seq(2013,2018,1)) 

ccplot + vdhplot + plot_layout(guides='collect')
