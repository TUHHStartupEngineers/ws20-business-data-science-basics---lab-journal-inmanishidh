# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

# 2.0 Importing Files ----
# A good convention is to use the file name and suffix it with tbl for the data structure tibble
bikes_tbl      <- read_excel(path = "C:/datascience/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/datascience/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")



# Not necessary for this analysis, but for the sake of completeness
bikeshops_tbl  <- read_excel("C:/datascience/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()



# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("City","State"),
           sep    = ",") %>%
  
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("State"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
  
# 6.0 Business Insights ----
# 6.1 Sales by Location ----
# Step 1 - Manipulate

sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
  select(State, total_price) %>%
  group_by(State) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_location_tbl




# Step 2 - Visualize
  sales_by_location_tbl
  ggplot(sales_by_location_tbl,aes(x = State, y = sales))+ 
  geom_col(fill = "#2DC6D6")+  # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by location",
    subtitle = "Upward Trend",
    x = "State", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
  sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
   select(order_date, total_price, State) %>%
   mutate(year = year(order_date)) %>%
   group_by(year, State) %>%
   summarise(sales = sum(total_price)) %>%
   ungroup() %>%
   mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))
  sales_by_year_state_tbl


# Step 2 - Visualize
  sales_by_year_state_tbl %>%
    ggplot(sales_by_year_state_tbl,mapping=aes(year,sales, fill=State)) +
    geom_col() +
    facet_wrap(~ State)+
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                      decimal.mark = ",", 
                                                      prefix = "", 
                                                      suffix = " €")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(
      title = "Revenue by year and State",
      fill = "State" # Changes the legend name
    )
