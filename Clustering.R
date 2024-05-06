

#Libraries
install.packages("tidyverse")
install.packages("openxlsx")
install.packages("readxl")
library(tidyverse)
library(openxlsx)
library(readxl)
library(tidyverse)
library(factoextra)
library(cowplot)
library(ggpubr)
library(cluster)
library(dplyr)
install.packages("stats")
library (stats)
install.packages("mclust")
library(mclust)
install.packages("factoextra")
library(factoextra)

#Upload final version of the file
library(readxl)
online_retail_fv <- read_excel("C:\\Users\\juanj\\Documents\\Predictive analytics\\Final\\online_retail_fv.xlsx")
View(online_retail_fv)

by_customer <- online_retail_fv %>% group_by(`Customer ID`) %>%
  summarize (Total_sales = sum(Total))

View (by_customer)

#Create the model by customer
kmeans_model <- kmeans(by_customer, centers = 4)


summary(kmeans_model)

by_customer$cluster <- kmeans_model$cluster

View(by_customer)


write.xlsx(by_customer, 'C:\\Users\\juanj\\Documents\\Predictive analytics\\Final\\customers.xlsx')



#Print model information from the original subset
print(kmeans_model)


#Plot the model with two graphic versions from the original subset
fviz_cluster(kmeans_model, data = by_customer )




