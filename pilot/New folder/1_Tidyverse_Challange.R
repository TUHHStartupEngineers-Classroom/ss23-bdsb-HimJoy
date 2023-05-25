# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)

library(readxl)

library("writexl")

library(lubridate)


# 2.0 Importing Files ----

bikes_tbl <- read_excel(path = "00_data/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("00_data/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

orderlines_tbl

glimpse(orderlines_tbl)

# 4.0 Joining Data ----
# Chaining commands with the pipe and assigning it to order_items_joined_tbl

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

# All actions are chained with the pipe already. Store the result in a variable at the end of the steps.

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ",")%>%
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity)%>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender)%>% 
  
  # 5.3.4 reorder the data by selecting the columns in your desired order.
  # use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("city"), contains("state"),
         price, quantity, total.price,
         everything())%>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----


# Step 1 - Manipulate

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(state, total_price) %>%
  
  # Grouping by state and summarizing sales
  group_by(state)%>% 
  summarize(sales = sum(total_price))%>% 
  
  # Optional: Add a column that turns the numbers into a currency format 
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_tbl

# Step 2 - Visualize

sales_by_state_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE)+  # Adding a trendline
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x-axis labels
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €"))+
  labs(
    title    = "Revenue by State",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and state ----

# Step 1 - Manipulate

sales_by_year_state_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_state_1_tbl

# Step 2 - Visualize

sales_by_year_state_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  #adding a treandline
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  #rotate x-axis label
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Facet
  facet_wrap(~ state)+
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €"))+
  labs(
    title = "Revenue by year and state",
    subtitle = "Not all state has an upward trend",
    fill = "States" # Changes the legend name
  )

# 7.0 Writing Files ----

# 7.1 Excel ----


bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wragled_data_tidy_ch/bike_orderlines.xlsx")

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wragled_data_tidy_ch/bike_orderlines.csv")

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wragled_data_tidy_ch/bike_orderlines.rds")
