library(tidyverse)

day3 <- read_csv("day3_input.txt", col_names = c("raw_inputs"))

tree_ck <- function(string, rownum, trees_right, trees_down){
  if (trees_down == 1)
    {if (str_sub(string, 1 + (rownum*trees_right) %% 31, 1 + (rownum*trees_right) %% 31) == "#") 1 else 0}
  else
    {if (rownum %% 2 == 0 & str_sub(string, (rownum/2+1) %% 31, (rownum/2+1) %% 31) == "#") 1 else 0}
}

day3_parsed <- day3 %>% 
  mutate(rownum = row_number() - 1,                                     #doesn't move in the first row
         tree1_flag = pmap_dbl(list(raw_inputs, rownum, 1, 1), tree_ck),
         tree3_flag = pmap_dbl(list(raw_inputs, rownum, 3, 1), tree_ck),
         tree5_flag = pmap_dbl(list(raw_inputs, rownum, 5, 1), tree_ck),
         tree7_flag = pmap_dbl(list(raw_inputs, rownum, 7, 1), tree_ck),
         tree12_flag = pmap_dbl(list(raw_inputs, rownum, 1, 2), tree_ck))
         
day3_parsed %>% 
  summarise(sum(tree1_flag),
            sum(tree3_flag),
            sum(tree5_flag),
            sum(tree7_flag),
            sum(tree12_flag))