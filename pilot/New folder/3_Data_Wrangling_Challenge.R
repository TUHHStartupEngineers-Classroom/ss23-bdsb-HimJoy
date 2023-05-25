# 1.0 Load libraries ----

library(tidyverse)
library(vroom)
library(magrittr)
library(lubridate)
library(data.table)



# 2.0 DATA IMPORT ----
patent <-  "00_data/04_Patent_data_reduced/patent.tsv"
patent_tbl <- fread(patent)
setnames(patent_tbl, "id", "patent_id")

assignee  <-   "00_data/04_Patent_data_reduced/assignee.tsv"
assignee_tbl <- fread(assignee)
setnames(assignee_tbl, "id", "assignee_id")

patent_assignee <- "00_data/04_Patent_data_reduced/patent_assignee.tsv"
patent_assignee_tbl<- fread(patent_assignee)

uspc <- "00_data/04_Patent_data_reduced/uspc.tsv"
uspc_tbl<- fread(uspc)



# 3.0 Patent Dominance ----
assignee_patentAssignee_merged <- merge(assignee_tbl, patent_assignee_tbl, by='assignee_id')
na.omit(assignee_patentAssignee_merged, cols="organization")

# US company with most patents
assignee_patentAssignee_merged [, .N, by = organization][order(-N)] %>% head(1)%>%na.omit()

# 10 US companies with most assigned/granted patents
assignee_patentAssignee_merged [, .N, by = organization][order(-N)]%>%na.omit() %>% head(10)




# 4.0 Recent patent activity ----
assignee_patentAssignee_patent_merged <- merge(assignee_patentAssignee_merged, patent_tbl, by='patent_id') 
assignee_patentAssignee_patent_merged_view <- assignee_patentAssignee_patent_merged[1:2]

# US company with most patents granted in august 2014
assignee_patentAssignee_patent_merged [lubridate::year(date) == 2014 & month(date) == 08, .N, by = organization][order(-N)]%>%na.omit() %>% head(1)

# 10 companies with most new granted patents in august 2014
assignee_patentAssignee_patent_merged [lubridate::year(date) == 2014 & month(date) == 08, .N, by = organization][order(-N)]%>%na.omit() %>% head(10)




# 5.0 Innovation in Tech ----
assignee_patentAssignee_uspc_merged <- merge(assignee_patentAssignee_merged, uspc_tbl, by='patent_id')
assignee_patentAssignee_uspc_merged_view <- assignee_patentAssignee_uspc_merged[1:2]

# Most innovative tech sector
assignee_patentAssignee_uspc_merged[, .N, by = type][order(-N)] %>% head(1)

# Top 5 USPTO main classes of their patents
assignee_patentAssignee_uspc_merged[organization=="International Business Machines Corporation", .N, by = mainclass_id][order(-N)]%>% head(5)
