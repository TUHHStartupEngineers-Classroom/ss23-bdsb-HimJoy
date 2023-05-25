# 1.0 Load libraries ----

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(scales)
library(forcats)
library(ggrepel)
library(data.table)
library(lubridate)
library(dplyr)
library(glue)


# 2.0 DATA IMPORT ----

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# 3.0 DATA filter ----

covid_data_tbl_compressed <- covid_data_tbl %>%
  
  filter((
            location == "Germany" | 
            location == "United_Kingdom" | 
            location == "France" | 
            location == "Spain" | 
            location == "United States"))

# Filter data until May 31, 2022
covid_data_tbl_compressed <- covid_data_tbl_compressed %>%
  filter(date <= as.Date("2022-05-31"))


# 4.0 DATA wrangling ----

#Modify dates
covid_data_tbl_compressed$date <- ymd(covid_data_tbl_compressed$date)
covid_data_tbl_compressed$year <- year(covid_data_tbl_compressed$date)
covid_data_tbl_compressed$month <- month(covid_data_tbl_compressed$date)
covid_data_tbl_compressed$month <- month.abb[month(covid_data_tbl_compressed$date)]
covid_data_tbl_compressed$day <- day(covid_data_tbl_compressed$date)

covid_data_tbl_compressed['month_year'] <- paste(covid_data_tbl_compressed$month, covid_data_tbl_compressed$year, sep = "'")

#Grouping and total cases calculation
covid_data_tbl_final<- covid_data_tbl_compressed %>% 
  group_by(location, month_year)%>%
  summarise(total_case = sum(total_cases)) %>%
  ungroup()


#Convert NULL value into 0 to calculate cumulative case
covid_data_tbl_final$total_case <- ifelse(is.na(covid_data_tbl_final$total_case), 0, covid_data_tbl_final$total_case)

#calculate cumulative case
covid_data_cumulative_case_tbl <- covid_data_tbl_final %>% mutate(cumulative_case=cumsum(total_case))


# 5.0 DATA visulization ----
covid_data_cumulative_case_tbl %>%
  
  ggplot(aes(month_year, cumulative_case, group=location, color = location)) +
  
  geom_line(size=.5) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €"))+
  
  labs(
    title = "Confirmed COVID-19 cases worldwide",
    subtitle = "As of 19/04/2020"
  )+
  theme_minimal() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45)) 