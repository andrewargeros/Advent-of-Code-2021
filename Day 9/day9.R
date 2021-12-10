library(tidyverse)

data = tibble(d = read_lines("C:/RScripts/Advent-of-Code-2021/Day 9/input.txt"))

# Part One ----------------------------------------------------------------------------------------

names = map(0:str_length(as.character(data[1,1])), ~paste0('A', .x)) %>% unlist()

d = data %>% 
  separate(d, names, sep = "") %>% 
  mutate_all(as.numeric) %>% 
  select(-A0)

sum_ = 0
for (i in 1:nrow(d)){
  for (j in 1:ncol(d)){
    center = d[i,j] %>% as.integer()
    
    above = ifelse(i > 1, d[i-1, j] %>% as.integer(), 10)
    below = ifelse(i < 100, d[i+1, j] %>% as.integer(), 10)
    left = ifelse(j > 1, d[i, j-1] %>% as.integer(), 10)
    right = ifelse(j < 100, d[i, j+1] %>% as.integer(), 10)
    
    all = c(above, below, right, left)
    
    if(all(all > center)){
      sum_ = sum_ + center + 1
    }
  }
}

sum(sum_)
