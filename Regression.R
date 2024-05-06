library (readxl)
library(caret)
library(ggplot2)
library(lattice)
library(dplyr)
install.packages("tsibble")
library(tsibble)
library(forecast)

# prepares sales data by month
sales_month <- online_retail_fv2 %>% mutate(InvoiceDate=yearmonth(InvoiceDate))


#data frames by location
sales_UK <- sales_month %>% filter(Country == 'United Kingdom')
sales_inter <- sales_month %>% filter(Country != 'United Kingdom')  


sales_month_UK <- sales_UK %>% group_by(InvoiceDate) %>% 
  summarize(Total_purchases = sum(Total))
sales_month_inter <- sales_inter %>% group_by(InvoiceDate) %>% 
  summarize(Total_purchases = sum(Total))


#delete Dec 2011 since it has only 4 days
sales_month_UK <- sales_month_UK[-25,]
sales_month_inter <- sales_month_inter[-25,]


#First hyphotis linear regression
linear_reg_UK <- lm (Total ~ InvoiceDate, data = sales_month)
summary(linear_reg_UK)

ggplot (sales_month_UK, aes(x = InvoiceDate, y = Total_purchases)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

linear_reg_inter <- lm (Total ~ InvoiceDate, data = sales_month)
summary(linear_reg_inter)

ggplot (sales_month_inter, aes(x = InvoiceDate, y = Total_purchases)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")


#Second hyphotis Forecasting based on ARIMA
ts_sales_UK <- ts(sales_month_UK$Total_purchases, start = 2010, frequency = 12)
plot(ts_sales_UK)

model_ts_UK <- auto.arima(ts_sales_UK)
forecast_sales_UK <- forecast(model_ts_UK, level = c(95), 12)
plot(forecast_sales_UK)

ts_sales_inter <- ts(sales_month_inter$Total_purchases, start = 2010, frequency = 12)
plot(ts_sales_inter)

model_ts_inter <- auto.arima(ts_sales_inter)
forecast_sales_inter <- forecast(model_ts_inter, level = c(95), 12)
plot(forecast_sales_inter)

summary (model_ts_UK)

#Third hyphotesis Seasonal ARIMA (time series)
model_sarima_UK <- arima(ts_sales, order = c(1,1,1), seasonal = list(order = c(0,1,1),
                                                                  period = 12))
forecast_sales_sar <- forecast(model_sarima_UK, level = c(95), 12)
plot(forecast_sales_sar)

summary(model_sarima_UK)



