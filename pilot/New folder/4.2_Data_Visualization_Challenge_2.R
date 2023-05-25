# 1.0 Load libraries ----

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(scales)
library(forcats)
library(ggrepel)
library(maps)

# 2.0 DATA IMPORT ----

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
world <- map_data("world")

# 3.0 DATA wrangling ----

#formatting countries&territories
covid_data_tbl <- covid_data_tbl%>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    countriesAndTerritories == "Bonaire, Saint Eustatius and Saba" ~ "Bonaire",
    countriesAndTerritories == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ countriesAndTerritories
  ))


covid_data_tbl <- covid_data_tbl %>%
  rename(region=countriesAndTerritories)

#Grouping and calculating mortality rate

covid_data_tbl <- covid_data_tbl %>%
  group_by(region)%>%
  summarise(total_deaths = sum(deaths), population = mean(popData2019)) %>%
  mutate(mortality_rate = (total_deaths / population)*100) %>%
  ungroup()

# 4.0 DATA Joining ----
world_covid_data_tbl <- dplyr::left_join(world, covid_data_tbl, by=c("region"))


# 5.0 DATA visulization ----

world_covid_data_tbl %>%
  ggplot(aes(map_id = region)) +
  geom_map(aes(fill = mortality_rate), map = world, color = "white") +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "Around 6.2 million confiemed COVID-19 deaths worldwide",
    caption = "Date: 04/16/2021"
  ) +
  expand_limits(x = world_covid_data_tbl$long, y = world_covid_data_tbl$lat) +
  scale_fill_gradient(high = "#541e2b", low = "#ff8282", na.value = "grey50",  guide = "colorbar")