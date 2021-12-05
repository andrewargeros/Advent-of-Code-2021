# Day 4 -------------------------------------------------------------------------------------------
# Giant Squid

library(tidyverse)

rep_board = function(d){
  d$newcol =  rep(1:ceiling(nrow(d)/5), each = 5)[1:nrow(d)]
  d
}

calls = c(72,99,88,8,59,61,96,92,2,70,1,32,18,10,95,33,20,31,66,43,26,24,91,44,11,15,48,90,27,29,14,68,3,50,69,74,54,4,16,55,64,12,73,80,58,83,6,87,30,41,25,39,93,60,9,81,63,75,46,19,78,51,21,28,94,7,17,42,53,13,97,98,34,76,89,23,86,52,79,85,67,84,47,22,37,65,71,49,82,40,77,36,62,0,56,45,57,38,35,5)

boards = read_lines("C:/RScripts/Advent-of-Code-2021/Day 4/input.txt",
                    skip = 2) %>% 
  tibble() %>% 
  rename(raw = 1) %>% 
  mutate(raw = str_trim(raw) %>% str_replace_all("  ", " ")) %>% 
  separate(1, c("B", "I", "N", "G", "O"), sep = " ") %>% 
  filter(!is.na(O)) %>% 
  mutate(index = lag(row_number() %/% 5) %>% replace_na(0))

## Part Two ---------------------------------------------------------------------------------------

mark_board = function(board, calls){
  board = board %>% mutate_all(~ifelse(.x %in% calls, NA, .x))
  return(board)
}

check_bingo = function(board){
  row = board %>% 
    rowwise() %>% 
    mutate(sna = sum(is.na(c_across()))) %>%
    ungroup() %>% 
    summarise(m = max(sna)) %>% 
    as.integer()
  
  col = board %>% 
    summarise(across(everything(), ~sum(is.na(.x)))) %>% 
    rowwise() %>% 
    summarise(sna = max(c_across())) %>% 
    as.numeric()
  
  if(row == 5 | col == 5){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

boards2 = boards

for (group in split(calls, ceiling(seq_along(calls)/5))){
  
  for (i in 0:max(boards$index)){
    board = boards2 %>% 
      filter(index == i) %>% 
      select(-index) %>% 
      mark_board(group)
    
    if (check_bingo(board)){
      break
    }
    
    boards2 = boards2 %>% 
      filter(index != i) %>% 
      bind_rows(board %>% mutate(index = i)) %>% 
      arrange(index)
  }
  
  if (check_bingo(board)){
    print("Bingo Found")
    print(board)
    break
  }
}

# A tibble: 5 x 5
# B     I     N     G     O    
# <chr> <chr> <chr> <chr> <chr>
# 1 98    9     NA    17    NA   
# 2 39    15    NA    16    47   
# 3 80    41    NA    51    21   
# 4 54    42    NA    NA    NA   
# 5 37    NA    NA    62    68 

print(group)
print(i)

boards %>% filter(index == i)

# A tibble: 5 x 6
# B     I     N     G     O     index
# <chr> <chr> <chr> <chr> <chr> <dbl>
# 1 98    9     70    17    18       87
# 2 39    15    88    16    47       87
# 3 80    41    8     51    21       87
# 4 54    42    31    10    59       87
# 5 37    92    33    62    68       87

# Since 33 and 31 are both part of the bingo, 31 is the answer as it comes last in the calls

board %>% 
  gather() %>% 
  summarise(s = sum(as.numeric(value), na.rm = T)) %>% 
  as.numeric() * 31

# [1] 21607

## Part Two ---------------------------------------------------------------------------------------

boards3 = boards

for (group in split(calls, ceiling(seq_along(calls)/5))){
  
  for (i in unique(boards3$index)){
    board = boards3 %>% 
      filter(index == i) %>% 
      select(-index) %>% 
      mark_board(group)
    
    if (check_bingo(board)){
      boards3 = boards3 %>% 
        filter(index != i)
    } else {
      boards3 = boards3 %>% 
        filter(index != i) %>% 
        bind_rows(board %>% mutate(index = i)) %>% 
        arrange(index)
    }
  }
  
  if (n_distinct(boards3$index) == 1){
    print("Last Bingo Found")
    print(board)
    break
  }
}

# A tibble: 5 x 5
# B     I     N     G     O    
# <chr> <chr> <chr> <chr> <chr>
# 1 NA    NA    56    49    NA   
# 2 36    57    NA    NA    77   
# 3 NA    NA    35    NA    NA   
# 4 82    NA    NA    NA    NA   
# 5 45    NA    NA    NA    NA  

for (j in calls[which(calls == group[5]):length(calls)]){
  cj = c(j)
  board = board %>% mark_board(cj)
  
  if (check_bingo(board)){
    print("Bingo Found")
    print(board)
    break
  }
}

# A tibble: 5 x 5
# B     I     N     G     O    
# <chr> <chr> <chr> <chr> <chr>
# 1 NA    NA    56    NA    NA   
# 2 36    57    NA    NA    77   
# 3 NA    NA    35    NA    NA   
# 4 82    NA    NA    NA    NA   
# 5 45    NA    NA    NA    NA

board %>% 
  gather() %>% 
  summarise(s = sum(as.numeric(value), na.rm = T))*j
  
# 19012