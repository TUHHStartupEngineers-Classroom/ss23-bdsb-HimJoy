# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# main-navigation-category-with-tiles__link 
# main-navigation-category-with-tiles__link 

# 2.0 DATA IMPORT ----

url_home          <- "https://www.radon-bikes.de/"
# xopen(url_home) # Open links directly from RStudio to inspect them
# Read in the HTML for the entire webpage
html_home <- read_html(url_home)

# Web scrape the ids for the families
bike_cat_url_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".megamenu__item > a") %>%
  # ...and extract the information of the id attribute
  html_attr('href') %>%
  
  
  
  # Convert vector to tibble
  enframe(name = "position", value = "cat_subcat_url")  %>%

  # Add the domain, because we will get only the subdirectories
  mutate(
    full_url = glue("https://www.radon-bikes.de{cat_subcat_url}")
  )

bike_cat_url_tbl


for(i in 1:8) {
  showAllModelLink <- bike_cat_url_tbl$full_url[i]
  # xopen(showAllModelLink)
  htmlStuffs <- read_html(showAllModelLink)
  
  
  bikegrid_url <- htmlStuffs %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".a-button--large") %>%
    # ...and extract the information of the id attribute
    html_attr('href') 
  
  current_url <- bike_cat_url_tbl$full_url[i]
  bike_cat_url_tbl$full_url[i] <- glue("https://www.radon-bikes.de{bikegrid_url}")
}

bike_cat_url_tbl





final_model_price_tbl <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("name", "price")
colnames(final_model_price_tbl) <- x


for(i in 1:8) {
 
  finalBikeModelListLink <- bike_cat_url_tbl$full_url[i]
  # xopen(finalBikeModelListLink)
  htmlStuffs_final <- read_html(finalBikeModelListLink)
  
  
  final_model_names <- htmlStuffs_final %>%
    
    html_nodes(css = ".m-bikegrid__info .a-heading--small") %>%
    html_text() %>%
    enframe(name = "position", value = "name")
  
  final_model_names
  
  final_price <- htmlStuffs_final %>%
    
    html_nodes(css = ".m-bikegrid__price.currency_eur .m-bikegrid__price--active") %>%
    html_text() %>%
    enframe(name = "position", value = "price")
  
  final_price

# 3.0 DATA wrangling ---- 
  
  semifinal_model_price_tbl <- left_join(final_model_names, final_price) %>% 
    select(name, price)
  
  
  final_model_price_tbl <- rbind(final_model_price_tbl, semifinal_model_price_tbl)
   
}

final_model_price_tbl

final_model_price_tbl <- final_model_price_tbl%>% 
  mutate(across(name, str_replace_all, "\n", ""))

final_model_price_tbl <- final_model_price_tbl%>% 
  mutate(across(name, str_replace_all, "[ ]{2,}", ""))

final_model_price_tbl






