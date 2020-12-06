library(tidyverse)

day6 <- read_lines("day6_input.txt") 

day6df <- tibble(input_data = day6) %>% 
  mutate(blank = ifelse(input_data == "",1,0),
         group_id = cumsum(blank)+1) %>% 
  filter(!blank) %>% 
  group_by(group_id) %>% 
  mutate(person_id = row_number(group_id)) %>% 
  separate_rows(input_data, sep = "") %>%
  filter(input_data != '') %>% 
  summarise(unique_q = n_distinct(input_data)) %>%
  ungroup()

day6df %>% 
  summarise(sum(unique_q))

day6df2 <- tibble(input_data = day6) %>% 
  mutate(blank = ifelse(input_data == "",1,0),
         group_id = cumsum(blank)+1) %>% 
  filter(!blank) %>% 
  group_by(group_id) %>% 
  mutate(person_id = row_number(group_id),
         group_count = n()) %>% 
  separate_rows(input_data, sep = "") %>%
  filter(input_data != '') %>%
  ungroup() %>% 
  group_by(input_data, group_id, group_count) %>% 
  summarise(letter_count = n()) %>%
  ungroup() %>%
  filter(letter_count == group_count) %>% 
  group_by(group_id) %>% 
  summarise(unique_q = n_distinct(input_data)) %>% 
  ungroup()

day6df2 %>% 
  summarise(sum(unique_q))
