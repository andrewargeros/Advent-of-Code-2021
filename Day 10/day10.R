library(tidyverse)

data = tibble(d = read_lines("C:/RScripts/Advent-of-Code-2021/Day 10/input.txt"))

braces = tribble(
  ~open, ~close, ~points,
  "(", ")", 3,
  "[", "]", 57,
  "{", "}", 1197,
  "<", ">", 25137
) %>% 
  mutate(brace = paste0(open, close))

check_string = function(s, brackets = braces$brace){
  while(str_count(s, "\\(\\)|\\[\\]|\\<\\>|\\{\\}") > 0){
    for (brace in brackets){
      s = str_remove(s, fixed(brace))
    }
  }
  return(s)
}

find_bad_char = function(s){
  mismatch = braces %>% 
    select(open, points) %>% 
    crossing(braces %>% select(close, points), .name_repair = "unique") %>% 
    rename("lpt" = 2,
           "rpt" = 4) %>% 
    filter(lpt != rpt) %>% 
    mutate(regex = paste0(open, close)) %>% 
    select(regex)
  
  for (regex in mismatch$regex){
    if (str_count(s, fixed(regex)) > 0){
      return(regex)
      break
    }
  }
  return(NA)
}

data %>% 
  mutate(clean_str = map_chr(d, check_string),
         bad = map_chr(clean_str, find_bad_char),
         wrong_char = substr(bad,2,2)) %>% 
  left_join(., braces, by = c("wrong_char" = "close")) %>% 
  summarise(s = sum(points, na.rm = T)) %>% 
  as.numeric()

# [1] 240123

# Part Two ----------------------------------------------------------------------------------------

flip_string = function(s){
  for (i in 1:nrow(braces)){
    tbraces = braces %>% filter(row_number() == i)
    s = str_replace_all(s, fixed(tbraces$open), fixed(tbraces$close))
  }
  s = stringi::stri_reverse(s)
  return(s)
}

score_string = function(s){
  char_lookup = tribble(
    ~char, ~pt,
    ")", 1,
    "]", 2,
    "}", 3,
    ">", 4
  )
  score = 0
  for (char_ in strsplit(s,"")[[1]]){
    point = char_lookup %>% filter(char == char_) %>% select(pt) %>% as.numeric()
    score = score*5 + point
  }
  return(score)
}

data %>% 
  mutate(clean_str = map_chr(d, check_string),
         bad = map_chr(clean_str, find_bad_char)) %>% 
  filter(is.na(bad)) %>% 
  mutate(rev = map_chr(clean_str, flip_string),
         score = map_dbl(rev, score_string)) %>% 
  summarise(s = median(score)) %>% 
  as.numeric()

# [1] 3260812321