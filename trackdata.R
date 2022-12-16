library(tidyverse)
library(janitor)
library(lubridate)
install.packages('gapminder')
library(gapminder)
install.packages('ggpubr')
library(ggpubr)
library(dplyr)

results <- read.csv('results.csv')

head(results)

results_mod <- results %>% 
  filter(Event == '10000M Men') %>% 
  arrange(Year) %>% 
  mutate(Seconds = period_to_seconds(ms(Result))) %>%
  mutate(Pace = 10000 / Seconds ) %>% 
  clean_names()

medal_pace <- ggplot(data = results_mod) + 
  geom_point(mapping = aes(x = year, y = pace, color=medal)) +
  facet_wrap(~medal) + 
  labs(title = '10000M: Pace Changes Over The Years')

medals_nation <- ggplot(data=results_mod) +
  geom_bar(mapping = aes(x = nationality, fill = medal)) +
  labs(title = "Medal Count by Nation in the Mens 10000M")

results_mod_sprint <- results %>% 
  arrange(result) %>% 
  filter(Event == '100M Men') %>% 
  slice(1:80) %>% 
  mutate(Result = as.numeric(Result)) %>% 
  clean_names()
 

  


sprint_speed <- ggplot(data= results_mod_sprint) +
  geom_point(mapping = aes(x = year, y = result, color = medal)) +
  facet_wrap(~medal) +
  labs(title= '100m: Medal Times Over The Years')

medals_nation_sprint <- ggplot(data=results_mod_sprint) +
  geom_bar(mapping = aes(x = nationality, fill = medal)) +
  labs(title = "Medal Count by Nation in the Mens 10000M")
 

medals_nation
medal_pace
sprint_speed
medals_nation_sprint




