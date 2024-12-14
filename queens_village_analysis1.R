#Neighborhood Assigned = Queens Village
# Import dataset into R 
install.packages('tidyverse')
library(tidyverse)
BOROUGH <- BOROUGH
BUILDING_CLASS <- BUILDING_CLASS
NEIGHBORHOOD <- NEIGHBORHOOD
NYC_TRANSACTION_DATA <- NYC_TRANSACTION_DATA
all_nyc <- NYC_TRANSACTION_DATA %>% left_join(BUILDING_CLASS, by =c("BUILDING_CLASS_FINAL_ROLL" =
                                                                      "X.BUILDING_CODE_ID")) %>% left_join(NEIGHBORHOOD, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID"))
View(all_nyc)
queens_village <- filter(all_nyc, NEIGHBORHOOD_NAME == "QUEENS VILLAGE")

 
nrow(queens_village)
#Filtering to check the number of residential properties in Queens Village
res <- filter(queens_village, TYPE == "RESIDENTIAL")
nrow(res)

#Generating histogram to show the distribution of sale prices in Queens Village:
library(ggplot2)
ggplot(res, aes(x = SALE_PRICE)) + geom_histogram(binwidth = 100000, fill = 'blue', color = 'black') + labs(title = 'Distribution of Sale Prices in Queens Village', x = "Sale Prices in USD", y = 'Count of Properties') 
#Generating histogram after filtering the dataset so sale prices > 0:
res_filt <- filter(res, SALE_PRICE > 0)
summary(res_filt$SALE_PRICE)
ggplot(res_filt, aes(x = SALE_PRICE)) + geom_histogram(binwidth = 100000, fill = 'blue', color = 'black') + labs(title = 'Distribution of Sale Prices in Queens Village', x = "Sale Prices in USD", y = 'Count of Properties')

res_filt <- res_filt %>%
  mutate(year = format(SALE_DATE, "%Y"))
latest_year_data <- filter(res_filt, year == '2021')
total_rev_2021 <- sum(latest_year_data$SALE_PRICE)
comm_earned_2021 <- total_rev_2021 * 0.05
company_rev_2021 <- comm_earned_2021 * 0.125
#Listing the required results
total_rev_2021
comm_earned_2021
company_rev_2021

#Creating a new column for Quarters:
res_filt <- res_filt %>%
  mutate(quarter = quarter(SALE_DATE))
#Renaming quarters to seasons:
res_filt <- res_filt %>%
  mutate(season = case_when(quarter == 1 ~ 'Winter', quarter == 2 ~ 'Spring', quarter == 3 ~ 'Summer', quarter == 4 ~ "Fall"))
#Creating a bar plot for sales by season/quarter
library(ggplot2)
ggplot(res_filt, aes(x = season)) + geom_bar(fill = 'purple') + labs(title = 'Count of Sales by Season/Quarter', x = 'Season', y = "Number of Sales")

#Summarize Sales Volume and Total revenue by Year
sales_trend <- res_filt %>%
  group_by(year) %>%
  summarise(total_sales_vol = n(), total_sales_rev = sum(SALE_PRICE, na.rm = TRUE))
sales_trend
#Creating Line Plot for Sales Volume
sales_trend$year <- as.numeric(sales_trend$year)
ggplot(sales_trend, aes(x = year, y = total_sales_vol)) + geom_line(color = 'blue', size = 1) + labs(title = 'Sales Volume Over the Years', x = 'Year', y = "Number of Sales")
#Creating Line Plot for Total Revenue
ggplot(sales_trend, aes(x = year, y = total_sales_rev)) + geom_line(color = 'green', size = 1) + labs(title = 'Total Revenue Over the Years', x = 'Year', y = "Total Revenue (USD)")
