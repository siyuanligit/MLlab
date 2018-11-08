#John Bentley 2018 NYCDSA ML lab

library(ggplot2)
library(lubridate)
library(dplyr)

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





