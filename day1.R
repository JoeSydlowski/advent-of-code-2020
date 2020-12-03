library(tidyverse)

day1 <- read_csv("day1_input.txt") %>% as_vector()

for (i in day1)
{
  for (j in day1)
  {
    if (i + j == 2020) print(paste0(i," + ",j," = 2020. The answer is ", i*j))
  }
}

for (i in day1)
{
  for (j in day1)
  {
    for(k in day1)
    {
      if (i + j +k == 2020) print(paste0(i," + ",j," + ",k," = 2020. The answer is ", i*j*k))
    }
  }
}