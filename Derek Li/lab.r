# working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/Week7/Machine_Learning_Lab/MLlab/Derek Li")

# dependencies
library(tidyverse)
library(lubridate)
library(psych)

### Part I
# problem 1
orders = read.csv("../data/Orders.csv", stringsAsFactors = FALSE)
orders$Sales = as.numeric(gsub("[$,]", "", orders$Sales))
orders$Profit = as.numeric(gsub("[$,]", "", orders$Profit))

# problem 2
names(orders)
orders$Ship.Date = mdy(orders$Ship.Date)
orders$Order.Date = mdy(orders$Order.Date)

orders %>% 
    mutate(order.month = month(Order.Date, label = TRUE),
           ship.month = month(Ship.Date, label = TRUE)) %>% 
    group_by(order.month) %>% 
    summarise(inventory = sum(Quantity)) %>% 
    ggplot(aes(x = order.month, y = inventory, fill = order.month)) +
    geom_col()

orders %>% 
    mutate(order.month = month(Order.Date, label = TRUE),
           ship.month = month(Ship.Date, label = TRUE)) %>% 
    group_by(order.month, Category) %>% 
    summarise(inventory = sum(Quantity)) %>% 
    ggplot(aes(x = order.month, y = inventory, fill = order.month)) +
    geom_col() +
    facet_grid(Category~.)

# problem 3
returns = read.csv("../data/Returns.csv", stringsAsFactors = FALSE)
returns = returns %>% 
    mutate(Region = case_when(Region == "Eastern Canada" ~ "Canada",
                              Region == "Western Canada" ~ "Cananda",
                              TRUE ~ Region))

returns %>% 
    left_join(orders, by = "Order.ID") %>% 
    group_by(year(Order.Date)) %>% 
    summarise(loss = sum(Profit))

returns %>% 
    left_join(orders, by = "Order.ID") %>% 
    group_by(Customer.ID) %>% 
    summarise(nreturn = n()) %>% 
    filter(nreturn > 1) %>% 
    nrow()

returns %>% 
    left_join(orders, by = "Order.ID") %>% 
    group_by(Customer.ID) %>% 
    summarise(nreturn = n()) %>% 
    filter(nreturn > 5) %>% 
    nrow()

orders %>% 
    select(Order.ID, Region) %>% 
    distinct() %>% 
    left_join(returns, by = c("Order.ID", "Region")) %>% 
    group_by(Region) %>% 
    summarise(rt = sum(!is.na(Returned)), rf = sum(is.na(Returned))) %>% 
    mutate(p = rt/(rt+rf)) %>% 
    ggplot(aes(x = Region, y = p)) +
    geom_col() +
    coord_flip()

orders %>% 
    left_join(returns, by = "Order.ID") %>% 
    group_by(Sub.Category) %>% 
    summarise(rt = sum(!is.na(Returned)), rf = sum(is.na(Returned))) %>% 
    mutate(p = rt/(rt+rf))

### Part II
# problem 4
# Step 1
orders %>% 
    left_join(returns, by = "Order.ID") %>% 
    mutate(Returned = case_when(is.na(Returned) ~ 0, TRUE ~ 1)) -> orders_return

# Step 2
orders_return %>% 
    mutate(Process.Time = as.numeric(Ship.Date - Order.Date)) -> orders_return

# Step 3
orders_return %>% 
    group_by(Product.ID) %>% 
    filter(Returned == 1) %>% 
    summarise(Return.Times = n()) -> returnTime

orders_return %>% 
    left_join(returnTime, by = "Product.ID") %>% 
    mutate(Return.Times = ifelse(is.na(Return.Times), 0, Return.Times)) -> orders_return

# problem 5
df = orders_return %>% 
    select(-contains("ID"), -Region.y) %>% 
    mutate(Ship.Mode = as.factor(Ship.Mode),
           Segment = as.factor(Segment),
           Region = as.factor(Region.x),
           Market = as.factor(Market),
           Category = as.factor(Category),
           Sub.Category = as.factor(Sub.Category),
           Order.Priority = as.factor(Order.Priority),
           Returned = as.factor(Returned)) %>% 
    select(-Region.x)

set.seed(0)
trainId = sample(1:nrow(df), 0.8*nrow(df))
trainSet = df[trainId,]
testSet = df[-trainId,]

modelLogistic = glm(Returned~., data = trainSet, family = "binomial")

# use H2O.ai
library(h2o)
init = h2o.init(nthreads = -1)

train = as.h2o(trainSet)
test = as.h2o(testSet)

predictors = names(train[which(names(train) != "Returned")])
response = "Returned"

glm = h2o.glm(x = predictors, y = response, training_frame = train, 
              family = "binomial", alpha = 1, lambda = 0,
              nfolds = 10)
h2o.auc(h2o.performance(glm, test))
# 0.7236 AUC
glm@model$coefficients_table

# grid search
grid = seq(0.0016,0.0017,0.00001)
result = c()
for (lamb in grid){
    result = rbind(result, c(lamb, h2o.auc(h2o.performance(h2o.glm(x = predictors, y = response, training_frame = train, 
                                                                   family = "binomial", alpha = 1, lambda = lamb,
                                                                   nfolds = 10), test))))
}
result

lambda = 0.00167
glm_final = h2o.glm(x = predictors, y = response, training_frame = train, 
              family = "binomial", alpha = 1, lambda = lambda,
              nfolds = 10)
h2o.auc(h2o.performance(glm_final, test))
glm_final@model$coefficients_table
# 0.7416 AUC

gbm = h2o.gbm(x = predictors, y = response, training_frame = train,
              distribution = "bernoulli", ntrees = 500, max_depth = 10, seed = 1, nfolds = 10)
h2o.auc(h2o.performance(gbm, test))
# 0.8390 AUC

rf = h2o.randomForest(x = predictors, y = response, training_frame = train, ntrees = 500, nfolds = 10)
h2o.auc(h2o.performance(rf, test))
# 0.8576 AUC