library(tidyverse)

check_pass <- function(min, max, rule, string) {
  if (between(str_count(string, rule), min, max)) 1
  else 0
}

check_loc <- function(min, max, rule, string) {
  if ( xor(substring(string, min, min) == rule, substring(string, max, max) == rule )) 1
  else 0
}

day2 <- read_csv("day2_input.txt", col_names = c("raw_inputs"))

day2_parsed <- day2 %>% 
  separate(raw_inputs, c("minmax", "rule", "password"), " ") %>% 
  separate(minmax, c("min", "max"), "-") %>% 
  mutate(rule = str_remove(rule, ":"),
         valid_pass = pmap_dbl(list(min, max, rule, password), check_pass),
         valid_pass_loc = pmap_dbl(list(min, max, rule, password), check_loc))

day2_parsed %>% 
  summarise(sum(valid_pass),
            sum(valid_pass_loc))
  