#John Bentley 2018 NYCDSA ML lab

library(ggplot2)
library(lubridate)
library(dplyr)
library(caret)

orders = read.csv("../data/Orders.csv")
returns = read.csv("../data/Returns.csv")

summary(orders)


#Part 1-1:
str(orders$Sales)
str(orders$Profit)
orders$Sales = as.character(orders$Sales)
orders$Profit = as.character(orders$Profit)
orders$Sales = as.numeric(gsub("[//$,]", '', orders$Profit))
orders$Profit = as.numeric(gsub("[//$,]", '', orders$Profit))

#Part 1-2:
str(orders$Order.Date)
orders$Order.Date = as.Date(orders$Order.Date, "%m/%d/%y")
orders$month = month(orders$Order.Date)
orders$year = year(orders$Order.Date)
his = ggplot(data = orders, aes(x = month, weight = Quantity)) + geom_bar()
his
his + facet_wrap(vars(year))
his + facet_grid(rows = vars(Category), cols = vars(year))

#Part 1-3-1:
orders_returns = left_join(orders, returns, by = "Order.ID")
names(orders_returns) = ifelse(names(orders_returns) == "Region.y",
                               "Return.Region", names(orders_returns))
names(orders_returns) = ifelse(names(orders_returns) == "Region.x",
                               "Buy.Region", names(orders_returns))
orders_returns$Returned = ifelse(is.na(orders_returns$Returned), 0,
                                 orders_returns$Returned)
orders_returns %>% filter(., Returned > 0) %>% group_by(., year) %>%
  summarise(returnedProfit = sum(Profit))

#Part 1-3-2:
customers_returns = orders_returns %>% group_by(., Customer.ID) %>%
  summarise(timesReturn = n())

  #Returned more than once
customers_returns %>% filter(., timesReturn > 1) %>% nrow(.)

  #Returned more than 5x
customers_returns %>% filter(., timesReturn > 5) %>% nrow(.)

#Part 1-3-3:
orders_returns %>% group_by(., Buy.Region) %>%
  summarise(., numBuys = n(), numReturns = sum(Returned)) %>%
  mutate(., Return.Rate = numReturns/numBuys) %>%
  arrange(., desc(Return.Rate))

#Part 1-3-4:
orders_returns %>% group_by(., Category) %>%
  summarise(., numBuys = n(), numReturns = sum(Returned)) %>%
  mutate(., Return.Rate = numReturns/numBuys) %>%
  arrange(., desc(Return.Rate))

orders_returns %>% group_by(., Category, Sub.Category) %>%
  summarise(., numBuys = n(), numReturns = sum(Returned)) %>%
  mutate(., Return.Rate = numReturns/numBuys) %>%
  arrange(., desc(Return.Rate))


# Part 2-4-1:
was_returned = orders_returns$Returned

# Part 2-4-2:
orders_returns$Ship.Date = as.Date(orders_returns$Ship.Date, "%m/%d/%y")
orders_returns$Process.Time = as.numeric(as.duration(orders_returns$Order.Date %--%
                                            orders_returns$Ship.Date)) / (24*60*60)

#Part 2-4-3:
orders_returns = orders_returns %>% group_by(., Product.ID) %>%
  mutate(., Num.Returns = sum(Returned)) %>% ungroup(.)

#Part 5
set.seed(123)

names(orders_returns)

MLdata = select(orders_returns, Ship.Mode, Segment, Postal.Code, Country,
                Buy.Region, Market, Category, Sub.Category, Sales, Quantity,
                Discount, Profit, Shipping.Cost, Order.Priority,
                month, Process.Time, Num.Returns, Returned)

#final cleaning
#missing vals:
colSums(is.na(MLdata))
MLdata$Postal.Code = ifelse(is.na(MLdata$Postal.Code), 0, MLdata$Postal.Code)
str(MLdata)
MLdata$Postal.Code = as.factor(MLdata$Postal.Code)
MLdata$month = as.factor(MLdata$month)
MLdata$Returned = as.factor(MLdata$Returned)


#make train - test split
trainIdx = createDataPartition(MLdata$Returned, p = .8)

#make randomforest model
library(randomForest)
#drop country and postal code because too many levels, info hopefully
#somewhat captured by region.
rfMLdata = MLdata[, !(names(MLdata) %in% c("Postal.Code","Country"))]

train = rfMLdata[trainIdx$Resample1,]
test = rfMLdata[-trainIdx$Resample1,]

rfModel1 = randomForest(Returned ~ ., data = rfMLdata, subset = trainIdx$Resample1,
                        importance = TRUE)

rfpredicts = predict(rfModel1, newdata = test)
table(rfpredicts, test$Returned)

#make boosting model
library(gbm)

gbmMLdata = select(rfMLdata, Sales, Quantity, Discount, Profit, Shipping.Cost,
                   Process.Time, Num.Returns, Returned)

gbmMLdata$Returned = as.character(gbmMLdata$Returned)

train = gbmMLdata[trainIdx$Resample1,]
test = gbmMLdata[-trainIdx$Resample1,]

boostModel1 = gbm(Returned ~ ., data = train, distribution = "bernoulli",
                  n.trees = 500, interaction.depth = 4)


boost_predicts = round(predict(boostModel1, newdata = test,
                         n.trees = 500, type = "response"))

table(boost_predicts, test$Returned)


#Lasso
train = rfMLdata[trainIdx$Resample1,]
test = rfMLdata[-trainIdx$Resample1,]

library(glmnet)


