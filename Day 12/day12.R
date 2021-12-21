library(tidyverse)

data = tibble(d = read_lines("C:/RScripts/Advent-of-Code-2021/Day 12/input.txt")) %>% 
  separate(d, c("input", "output"), sep = "-")

# Part One ----------------------------------------------------------------------------------------

data = data %>% 
  rename("output" = 1,
         "input" = 2) %>% 
  bind_rows(data)

options = data %>% filter(input == "start")

all_paths = tibble()
for (option in starting_paths$output){
  new_opts = data %>% filter(input == option)
  
  path = c(option)
  
  for (opt in new_opts$output){
    
    path = path %>% append(opt)
    
    if (opt == "end"){
      path = tibble(p = path) %>% summarise(p = paste0(p, collapse = ","))
      all_paths = all_paths %>% bind_rows(path)
    } else if (length(path) > 0 & opt == "start") {
      break
    } else {
      
    }
  }
  
}

find_next = function(out, visited){
  data %>% 
    filter(input == out) %>% 
    filter(str_detect(output, "[a-z]") & !output %in% visited)
}

all_paths = tibble()
path = c('start')
for (option in options$output){
  path = path %>% append(option)
  
  if (option == "end"){
    path = tibble(p = path) %>% summarise(p = paste0(p, collapse = ","))
    all_paths = all_paths %>% bind_rows(path)
    next
  } else if (length(path) > 0 & option == "start"){
    next
  } else {
    options = find_next(option, path)
    path = path %>% append(option)
  }
}



data %>% 
  left_join(., data, by = c("output" = "input"), keep = T)

data %>% filter(input == "kj")
