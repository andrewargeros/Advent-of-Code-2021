# Day 7 -------------------------------------------------------------------------------------------
# The Treachery of Whales

library(tidyverse)

data = read_lines("C:/RScripts/Advent-of-Code-2021/Day 7/input.txt") %>% 
  str_split(",") %>% 
  unlist() %>% 
  tibble() %>% 
  rename("initial" = 1) %>%
  mutate(initial = as.integer(initial))

# Part One ----------------------------------------------------------------------------------------

data %>% 
  mutate(dist = abs(initial - median(data$initial))) %>% 
  summarise(sum(dist)) %>% 
  as.numeric()

# [1] 336721

# Part Two ----------------------------------------------------------------------------------------

mult = function(x) x * (x-1) / 2 # Lol Discrete Math paid off

data %>% 
  mutate(dist = abs(initial - floor(mean(data$initial))),
         dist_m = mult(dist),
         dist = dist + dist_m) %>% 
  summarise(sum(dist)) %>% 
  as.numeric()

# [1] 91638945