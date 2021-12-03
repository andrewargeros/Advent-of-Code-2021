# Day 3 -------------------------------------------------------------------------------------------
# Binary Diagnostic

library(tidyverse)
options(scipen = 999)

data = read_lines("C:/RScripts/Advent-of-Code-2021/Day 3/data.txt") %>%
  tibble() %>% 
  rename(d = 1) 

# Part One ----------------------------------------------------------------------------------------

new_data = tibble()

for (d in data$d){
  row = str_split_fixed(d, n = 12, pattern = "") %>% as.tibble()
  
new_data = new_data %>% bind_rows(row)
}

mc = new_data %>% 
  mutate(across(everything(), as.integer)) %>% 
  summarise(across(everything(), ~sum(.x)/nrow(new_data))) %>% 
  mutate(across(everything(), ~ifelse(.x > 0.5, 1,0))) %>% 
  unite(d , everything(), sep = "") %>% 
  as.character() %>% 
  strtoi(base = 2)

lc = new_data %>% 
  mutate(across(everything(), as.integer)) %>% 
  summarise(across(everything(), ~sum(.x)/nrow(new_data))) %>% 
  mutate(across(everything(), ~ifelse(.x < 0.5, 1,0))) %>% 
  unite(d , everything(), sep = "") %>% 
  as.character() %>% 
  strtoi(base = 2)

mc*lc
# [1] 4103154

# Part Two ----------------------------------------------------------------------------------------

oxygen_data = new_data %>% mutate(across(everything(), as.integer))
co2_data = new_data %>%  mutate(across(everything(), as.integer))

for (col in names(new_data)){
  
  ox_num = oxygen_data %>% 
    select(col) %>% 
    summarise_all(mean) %>% 
    mutate_all(~ifelse(.x >= 0.5, 1, 0)) %>% 
    as.integer()
  
  oxygen_data = oxygen_data %>% filter(get(col) == ox_num)
  print(paste(col, nrow(oxygen_data)))
  
  if (nrow(oxygen_data) == 1){
    break
    }
}

for (col in names(new_data)){
  
  co2_num = co2_data %>% 
    select(col) %>% 
    summarise_all(mean) %>% 
    mutate_all(~ifelse(.x >= 0.5, 0, 1)) %>% 
    as.integer()
  
  co2_data = co2_data %>% filter(get(col) == co2_num)
  print(paste(col, nrow(co2_data)))
  
  if (nrow(co2_data) == 1){
    break
    }
}

ox_num = oxygen_data %>% 
  unite(d , everything(), sep = "") %>% 
  as.character() %>% 
  strtoi(base = 2)

co2_num = co2_data %>% 
  unite(d , everything(), sep = "") %>% 
  as.character() %>% 
  strtoi(base = 2)

ox_num*co2_num
# [1] 4245351