library(tidyverse)

# read data
df_t6 <- read_csv("data/underage_alcohol.csv")
df_mc <- read_csv("data/arrests_alcoholrelated.csv")

# join data
df <- left_join(df_mc, df_t6)

# compare
ggplot(df, aes(x = arr_rate, y = as.numeric(per_1000))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~locality, scales = "free")
# mine are systematically larger

# is pop the same?
df %>% 
  mutate(popdiff = pop_1019 - population) %>% 
  ggplot(aes(x = popdiff)) +
  geom_histogram() +
  facet_wrap(~locality, scales = "free")
# mine are always less -- are they using total pop? Nope, pop 17 and under

# look at counts
ggplot(df, aes(x = arrests_civreport, y = underage_arrests)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~locality, scales = "free")
# mine are systematically larger


pop <- read_csv("data/pop_data_cdc.csv")

ggplot(df, aes(x = year, y = arr_rate, color = locality)) +
  geom_point() + geom_line()
