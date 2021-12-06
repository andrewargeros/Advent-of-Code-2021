 # Day 6 ----------------------------------------------------------------------------------------
 ## Laternfish
 
 library(tidyverse)

 data = read_lines("C:/RScripts/Advent-of-Code-2021/Day 6/input.txt") %>% 
   str_split(",") %>% 
   unlist() %>% 
   tibble() %>% 
   rename("initial" = 1) %>%
   mutate(initial = as.integer(initial))

## Part One ---------------------------------------------------------------------------------------
 
 fish_move = function(value){
   if (value > 0) {
     return(value - 1)
   } else {
     return(6)
   }
 }

 fish_to_add = function(data){
   data %>% 
     select(last_col()) %>% 
     summarise_all(~sum(.x == 0)) %>% 
     as.numeric()
 }

 
 for (i in 1:80){
   add = data %>% fish_to_add()
   
   new_data = data %>% 
     select(last_col()) %>%
     rename('today' = 1) %>% 
     mutate(new = map(today, fish_move) %>% as.numeric()) %>% 
     bind_rows(tibble(new = rep(8, add))) %>% 
     select(new) %>% 
     rename_all(~paste0("day", i))
    
   data = data %>% qpcR:::cbind.na(new_data)
 }

 data %>% 
   select(day80) %>% 
   count() %>% 
   as.integer()

# [1] 346063 
 
 ## Part Two ---------------------------------------------------------------------------------------
 
 # Basically either approach works... Using the above might be a little more explainable but will 
 # chew up a ton of RAM for higher iterations. Below uses an aggregate table for each day, so it is
 # less computationally intensive than above
 
 basetype = tibble(value = 0:8) %>% 
    full_join(., data %>% count(initial), 
              by = c("value" = "initial")) %>% 
    mutate_all(~replace_na(.x, 0))
 
 for (i in 1:256){
    add = basetype %>% 
       filter(value == 0) %>% 
       select(last_col()) %>% 
       as.numeric() # Learned the hard way that big ints do not work for R... this cannot be as.integer()
    
    new_day = basetype %>% 
       select(value, last_col()) %>% 
       rename(n = 2) %>% 
       mutate(value = map(value, fish_move) %>% as.numeric()) %>% 
       group_by(value) %>% 
       summarise(n = sum(n)) %>% 
       bind_rows(tribble(~value, ~n,
                         8, add)) %>% 
       rename_at(2, ~paste0("day", i))
    
    basetype = basetype %>% 
       full_join(new_day, by = "value") %>% 
       mutate_all(~replace_na(.x, 0))
}

 basetype %>% 
    summarise(s = sum(day256)) %>% 
    as.numeric() %>% 
    scales::comma()

# [1] 1572358335990
 
## Just for Fun -----------------------------------------------------------------------------------
 
basetype %>% 
    select(2:last_col()) %>% 
    summarise_all(sum) %>% 
    gather() %>% 
    mutate(day = str_extract(key, "\\d*$") %>% as.numeric() %>% replace_na(0)) %>% 
    ggplot() +
    aes(x = day, y = value) +
    geom_line()
 