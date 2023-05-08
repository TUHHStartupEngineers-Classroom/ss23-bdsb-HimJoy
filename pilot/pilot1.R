library(tidyverse)

#tyding data with pivot_lonnger()
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)

diamonds2_long <- diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"),
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 6)

diamonds2_long

#liner regression anlysis
model <- lm(price ~ ., data = diamonds2_long)
model

?lm()

#tyding data with pivot_wider()
diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 8)

diamonds3_wider <- diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5) 

diamonds3_wider

#tyding data with seperate()
diamonds4 <- readRDS("diamonds4.rds")

diamonds4

?separate()

separated <- diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)
separated

#tyding data with unite()
separated %>% unite(dim, x,y,z, sep = '/', remove = T)

#tranform
#filtering

library(ggplot2) # To load the diamonds dataset
library(dplyr)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

#with slicer
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% slice(3:4)

?filter()

#rearranging data with arrange()
?arrange()

diamonds %>% 
  arrange(cut, carat, desc(price))

diamonds %>% 
  select(color, depth, x:y) %>% 
  head(n = 5)

diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)
