library(tidyverse)

day5 <- read_csv("day5_input.txt", col_names = c("raw_inputs"))

get_rc <- function(string, row_col) {
  
  rc_min <- 0
  rc_max <- if (row_col == "row") 128 else 8
  
  for (i in str_split(string,"")[[1]])
  {
    current_range <- rc_max - rc_min
    
    if (i == "F" | i == "L") rc_max <- rc_max - current_range / 2 else rc_min <- rc_min +  current_range / 2 
  }
  
  rc_max - 1
  
}



day5_df <- day5 %>%
  mutate(row_num = map2_dbl(str_sub(raw_inputs,1,7), "row", get_rc),
         col_num = map2_dbl(str_sub(raw_inputs,8,11), "col", get_rc),
         seat_id = row_num * 8 + col_num)

day5_df %>%
  summarise(max(seat_id))

day5_df %>% 
  filter(row_num != min(row_num) & row_num != max(row_num)) %>% 
  arrange(seat_id) %>% 
  mutate(min_id = min(seat_id),
         seat_index = row_number() -1 + min_id) %>% 
  filter(seat_id != seat_index) %>% 
  summarise(min(seat_index))