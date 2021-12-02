## Day 1 ------------------------------------------------------------------------------------------
# Dive! 

library(tidyverse)

data = read_csv("C:/RScripts/Advent-of-Code-2021/Day 2/data.csv")

# Part One ---------------------------------------------------------------------------------------- 

data %>% 
  mutate(direction = str_remove_all(d, "\\d") %>% str_trim(),
         amt = str_extract_all(d, "\\d$") %>% as.integer()) %>% 
  mutate(amt = ifelse(direction == "up", amt*-1, amt),
          xy = ifelse(direction != "forward", "y", "x")) %>% 
  group_by(xy) %>% 
  summarise(total = sum(amt))

# A tibble: 2 x 2
# xy    total
# <chr> <dbl>
# x      1909
# y       655

1909*655

# [1] 1250395

# Part Two ----------------------------------------------------------------------------------------

data %>% 
  mutate(direction = str_remove_all(d, "\\d") %>% str_trim(),
         amt = str_extract_all(d, "\\d$") %>% as.integer(),
         amt = ifelse(direction == "up", amt*-1, amt)) %>% 
  mutate(aim = ifelse(direction != "forward", amt, 0),
         aim = cumsum(aim)) %>% 
  filter(direction == "forward") %>% 
  mutate(depth = amt*aim) %>% 
  summarise(x = sum(amt),
            y = sum(depth))
  
# A tibble: 1 x 2
# x      y
# <dbl>  <dbl>
# 1909   760194

1909*760194
# [1] 1451210346