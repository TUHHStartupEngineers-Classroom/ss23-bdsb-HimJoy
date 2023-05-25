# 1.0 Load libraries ----

library(tidyverse)

# 2.0 DATA IMPORT ----

sw_api <- function(weatherUrl) {
  resp <- httr::GET(weatherUrl)
  httr::stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("https://api.openweathermap.org/data/2.5/onecall?lat=53.5511&lon=9.9937&appid=d93321800837c9ff4a16acd678b13513")
resp


# 3.0 DATA wrangling ----

rawToChar(resp$content)


resp %>% 
  .$content %>% 
  rawToChar() %>% 
  jsonlite::fromJSON()


