#John Bentley 2018 NYCDSA ML lab

library(ggplot2)
library(lubridate)

orders = read.csv("../data/Orders.csv")
returns = read.csv("../data/Returns.csv")

summary(orders)


#Part 1-1:
str(orders$Sales)
str(orders$Profit)
orders$Sales = as.numeric(orders$Sales)
orders$Profit = as.numeric(orders$Profit)

#Part 1-2:







