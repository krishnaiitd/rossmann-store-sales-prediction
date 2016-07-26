# Ross man store sales prediction

library(readr)
library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)

if(!exists("train")) {
  train <- read.csv("train.csv")
}

train$Date <- as.Date(train$Date)

print(nrow(train))
print(ncol(train))
print(head(train))

# find the number of store by date
stores_count_by_Date <- train %>% group_by(Date) %>% summarise(NumStores=n())

g <- ggplot(data = stores_count_by_Date, aes(x = Date, y = NumStores))
g <- g + geom_line()
g <- g + xlab('Date') + ylab('Numner of Stores')
g <- g + ggtitle('Number of store by date')
plot(g)
rm(g)
rm(stores_count_by_Date)

# find the reveune of each store
stores_revenue <- train %>% group_by(Store) %>% summarise(revenue = sum(Sales))
g <- ggplot(data = stores_revenue, aes(x= Store, y = revenue))
g <- g + geom_line()
g <- g + xlab('Stores') + ylab('Total Revenue')
g <- g + ggtitle('Revenue of stores')
plot(g)
rm(g)
rm(stores_revenue)


revenue_by_DayOfWeek <- train %>% group_by(DayOfWeek) %>% summarise(revenue = sum(Sales))
g <- ggplot(data = revenue_by_DayOfWeek, aes(x= DayOfWeek, y = revenue))
g <- g + geom_bar(stat="identity")
g <- g + xlab('Day of Week') + ylab('Total Revenue')
g <- g + ggtitle('Revenue on each days')
plot(g)
rm(g)
#rm(revenue_by_DayOfWeek)


