library(tidyverse)

day4 <- read_lines("day4_input.txt")

count_fn <- function(string){
  
  counter <- 0
  
  for (i in c("byr","iyr","eyr","hgt","hcl","ecl","pid")){
    counter <- counter + str_count(string, i)
  }
  
  counter
  
}

get_value <- function(string, var) {
  
  reg_str <- paste0(var,":[#a-zA-Z0-9_]*\\b")
  
  str_sub(str_sub(string, str_locate(string,reg_str)),5)
  
}

day4df <- str_split(str_replace_all(str_replace_all(paste(day4, collapse = ","),",,","\\|"),","," "),"\\|") %>%
  as_vector() %>%
  as_tibble() %>% 
  mutate(field_count = map_dbl(value, count_fn),
         byr = map2(value, "byr", get_value),
         iyr = map2(value, "iyr", get_value),
         eyr = map2(value, "eyr", get_value),
         hgt = map2(value, "hgt", get_value),
         hcl = map2(value, "hcl", get_value),
         ecl = map2(value, "ecl", get_value),
         pid = map2(value, "pid", get_value),
         valid_pass = if_else(between(byr,1920,2002) &
                                  between(iyr,2010,2020) &
                                  between(eyr,2020,2030) &
                                  ( (str_detect(hgt,"in") & between(str_sub(hgt, str_locate(hgt,"\\d*")),59,76)) |
                                    (str_detect(hgt,"cm") & between(str_sub(hgt, str_locate(hgt,"\\d*")),150,193)) ) &
                                  str_detect(hcl, "#[a-f0-9]{6}") &
                                  ecl %in% c("amb","blu","brn","gry","grn","hzl","oth") &
                                  str_detect(pid, "^[0-9]{9}$")
                                  ,1
                                  ,0)
  )
  

day4df %>% 
  summarise(sum(field_count == 7),
            sum(valid_pass, na.rm = TRUE))
