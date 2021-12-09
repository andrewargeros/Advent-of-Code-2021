library(tidyverse)

data = read_lines("C:/RScripts/Advent-of-Code-2021/Day 8/input.txt") %>% 
  tibble() %>% 
  rename(d = 1) %>% 
  mutate(input = str_extract(d, "(.*?)\\|") %>% str_remove(" \\|$") %>% str_trim(),
         output = str_extract(d, "\\|(.*?)$") %>% str_remove("^\\|") %>% str_trim())

# Part One ----------------------------------------------------------------------------------------

d = data %>% 
  select(output) %>% 
  str_split(" ") %>% 
  unlist() %>% 
  tibble() %>% 
  rename(out = 1) %>% 
  mutate(out2 = str_remove_all(out, 'c\\(|[^A-Za-z]'),
         len = str_length(out2)) %>% 
  filter(len %in% c(2, 4, 3, 7)) %>% 
  nrow()

# [1] 409

d %>% 
  mutate(f = alphabetize(out2)) %>% 
  select(len, f) %>% 
  distinct()

# Part Two ----------------------------------------------------------------------------------------

alphabetize = function(s) map(s, ~paste(sort(unlist(strsplit(.x, ""))), collapse = "")) %>% as.character()

str_diff = function(x,y){
  setdiff(stringr::str_split(x[[1]], "")[[1]], 
          str_split(y[[1]], "")[[1]] 
  )
}

for (i in 1:nrow(data)){
  row = data %>% filter(row_number() == i)
  
  input_dt = row %>% 
    select(input) %>% 
    as.character() %>% 
    str_split(' ') %>% 
    unlist()
  input_lens = map(input_dt, str_length) %>% 
    unlist() 
  
  one = input_dt[which(input_lens == 2)]
  seven = input_dt[which(input_lens == 3)]
  os_diff = str_diff(seven, one)
  
  fives = input_dt[which(input_lens == 5)]
}

input = read_lines('C:/RScripts/Advent-of-Code-2021/Day 8/input.txt')
splitter <- function(x) {
  str_split(x, " ") %>%
    map(str_split, "") %>%
    map(map, sort)
}

setdiff_length <- function(x, y) {
  lengths(map(x, ~setdiff(x[[which(y)]], .x)))
}

minus1 <- function(x) x - 1

solver <- function(lights, right) {
  x1 <- lengths(lights) == 2
  x4 <- lengths(lights) == 4
  x7 <- lengths(lights) == 3
  x8 <- lengths(lights) == 7
  x6 <- lengths(lights) == 6 & setdiff_length(lights, x1) == 1
  x0 <- lengths(lights) == 6 & setdiff_length(lights, x4) == 1 & !x6
  x9 <- lengths(lights) == 6 & !x6 & !x0
  x5 <- lengths(lights) == 5 & setdiff_length(lights, x6) == 1
  x3 <- lengths(lights) == 5 & setdiff_length(lights, x9) == 1 & !x5
  x2 <- lengths(lights) == 5 & !x5 & !x3
  
  cont <- list(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) %>%
    map(~lights[[which(.x)]]) %>%
    map(sort)
  
  right %>%
    match(cont) %>%
    minus1() %>%
    paste(collapse = "") %>%
    as.numeric()
}

t = tibble(input) %>%
  separate(input, c("left", "right"), sep = " \\| ") %>%
  mutate(across(c("left", "right"), splitter)) %>%
  mutate(res = map2_dbl(left, right, solver)) %>%
  summarise(total = sum(res))           
