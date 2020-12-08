library(tidyverse)


day7 <- read_lines("day7_input.txt")

global_shiny_gold_count <- 0
bag_join <- function(df){
  
  shiny_gold_flags <- df %>% 
    filter(shiny_gold_flag == 1) %>% 
    select(outer_bag, new_flag = shiny_gold_flag)
  
  other_bags <- df %>% 
    left_join(shiny_gold_flags, by = c("inner_bag1" = "outer_bag")) %>% 
    left_join(shiny_gold_flags, by = c("inner_bag2" = "outer_bag")) %>% 
    left_join(shiny_gold_flags, by = c("inner_bag3" = "outer_bag")) %>% 
    mutate(shiny_gold_flag = if_else(shiny_gold_flag == 1 | new_flag == 1 | new_flag.x == 1 | new_flag.y == 1, 1, 0)) %>% 
    select(outer_bag, inner_bag1, inner_bag2, inner_bag3, shiny_gold_flag)
  
  current_shiny_gold_count <- other_bags %>% summarise(sum(shiny_gold_flag, na.rm = TRUE)) %>% pull()
  
  if (global_shiny_gold_count != current_shiny_gold_count) {
    global_shiny_gold_count <<- current_shiny_gold_count
    print(global_shiny_gold_count)
    bag_join(other_bags)} else return(other_bags)

}

day7df <- tibble(data_input = day7) %>%
  mutate(data_input = str_remove_all(str_remove_all(data_input, '\\d\\s'), '\\.')) %>% 
  separate(col = data_input, into = c("outer_bag","inner_bags"), sep = " contain ") %>% 
  separate(col = inner_bags, into = c("inner_bag1","inner_bag2","inner_bag3"), sep = ", ") %>% 
  mutate(shiny_gold_flag = if_else(str_detect(inner_bag1, "shiny gold") |
                                   str_detect(inner_bag2, "shiny gold") |
                                   str_detect(inner_bag3, "shiny gold"), 1, 0))

bag_join(day7df) %>%
  filter(shiny_gold_flag == 1) %>% 
  select(outer_bag, shiny_gold_flag) %>% 
  unique() %>% 
  summarise(sum(shiny_gold_flag, na.rm = TRUE))


temp <- bag_join(day7df)


shiny_gold_flags <- day7df %>% 
  filter(shiny_gold_flag == 1) %>% 
  select(outer_bag, new_flag = shiny_gold_flag)

other_bags <- day7df %>% 
  left_join(shiny_gold_flags, by = c("inner_bag1" = "outer_bag")) %>% 
  left_join(shiny_gold_flags, by = c("inner_bag2" = "outer_bag")) %>% 
  left_join(shiny_gold_flags, by = c("inner_bag3" = "outer_bag")) %>% 
  mutate(shiny_gold_flag = if_else(shiny_gold_flag == 1 | new_flag == 1 | new_flag.x == 1 | new_flag.y == 1, 1, 0)) %>% 
  select(outer_bag, inner_bag1, inner_bag2, inner_bag3, shiny_gold_flag)

