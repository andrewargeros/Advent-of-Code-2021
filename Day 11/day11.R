library(tidyverse)

data = tibble(d = read_lines("C:/RScripts/Advent-of-Code-2021/Day 11/input.txt")) %>% 
  mutate(rowidx = row_number(),
         val = str_split(d, "")) %>% 
  unnest(val) %>% 
  mutate(val = as.numeric(val)) %>% 
  group_by(rowidx) %>% 
  mutate(colidx = row_number()) %>% 
  ungroup() %>% 
  select(-d)

# Part One ----------------------------------------------------------------------------------------

adjacent_pts = tibble(x = -1:1) %>% 
  crossing(tibble(y = -1:1))

adjacent = function(d1, d2){
  d1 %>% 
    crossing(adjacent_pts) %>% 
    mutate(row2 = rowidx + x,
           col2 = colidx + y) %>% 
    inner_join(., d2, by = c(row2 = "rowidx", col2 = "colidx"), suffix = c("", "2")) %>% 
    filter(rowidx != row2 | colidx != col2)
}

flashed_total = 0

for (i in 1:100){
  data = data %>% 
    mutate(val = val + 1,
           flash = val > 9,
           has_flashed = flash)
  
  while (sum(data$flash) > 0){
    flashed_total = flashed_total + sum(data$flash)
    
    data = data %>% 
      adjacent(data) %>% 
      group_by(rowidx, colidx, val, flash, has_flashed) %>% 
      summarise(val = first(val) + sum(flash2)) %>% 
      ungroup() %>% 
      mutate(flash = val > 9 & !has_flashed) %>% 
      mutate(has_flashed = flash | has_flashed)
    
    if (all(data$has_flashed | data$flash)){
      stop(paste0("Done Part 2: ", i))
    }
  }
  
  
  data = data %>% mutate(val = ifelse(has_flashed, 0, val))
}


# Part One 1702
# Part Two 251