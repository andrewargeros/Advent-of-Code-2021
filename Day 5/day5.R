# Day 5 -------------------------------------------------------------------------------------------
## Hydrothermal Venture

library(tidyverse)

data = tibble(d = read_lines("C:/RScripts/Advent-of-Code-2021/Day 5/input.txt")) %>% 
  mutate(d = str_remove_all(d, "->") %>% str_replace_all("  ", ",")) %>% 
  separate(d, c("x1", "y1", "x2", "y2")) %>% 
  mutate_all(as.integer)

# Part One ----------------------------------------------------------------------------------------

straight_lines = data %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  mutate(id = row_number())

test = straight_lines %>%  
  mutate(x = map2(x1,x2,~.x:.y),
         y = map2(y1,y2,~.x:.y)) %>%  # Hack-y R way to express a range of numbers -- all integers between y1 and y2
  select(id, x, y) %>%  
  unnest(c(x,y)) %>% 
  mutate(coord = paste(x,y)) %>% 
  count(coord) %>%    # Could also use group_by() %>% summarise()
  filter(n > 1) %>%   # Every point obv occurs once, get the ones with > 1
  count() %>% 
  as.numeric()

# [1] 7297

# Part Two  ----------------------------------------------------------------------------------------

## Holy this was lucky to work on the overarching data

data %>% 
  mutate(id = row_number()) %>%   # Just have to create the ID for the data
  mutate(x = map2(x1, x2, ~.x:.y),
         y = map2(y1, y2, ~.x:.y)) %>%  
  select(id, x, y) %>%  
  unnest(c(x,y)) %>% 
  mutate(coord = paste(x,y)) %>% 
  count(coord) %>% 
  filter(n > 1) %>% 
  count() %>% 
  as.numeric()

# [1] 21038