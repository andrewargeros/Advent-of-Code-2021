## Day 1 ------------------------------------------------------------------------------------------
# Increase/Decrease 

library(tidyverse)

data = read_csv("C:/RScripts/Advent-of-Code-2021/Day 1/data.csv")

## PART 1 -----------------------------------------------------------------------------------------

data %>% 
  mutate(l = lag(d),
         inc = ifelse(d - l > 0, "Increase", "Dont Care")) %>% 
  count(inc) %>% 
  filter(inc == "Increase")
  
# A tibble: 1 x 2
# inc          n
# <chr>    <int>
# Increase  1602

## PART 2 -----------------------------------------------------------------------------------------

data %>% 
  mutate(l1 = lag(d),
         l2 = lag(d, 2),
         s = d + l1 + l2) %>% 
  # filter(row_number() %% 3 == 0) %>% 
  mutate(l = lag(s),
         inc = ifelse(s - l > 0, "Increase", "Dont Care")) %>% 
  count(inc) %>% 
  filter(inc == "Increase")

# A tibble: 1 x 2
# inc          n
# <chr>    <int>
# Increase  1633