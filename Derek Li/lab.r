# working directory
setwd("C:/Users/Derek/Google Drive/bootcamp/Week7/Machine_Learning_Lab/MLlab/Derek Li")

# dependencies
library(tidyverse)
library(lubridate)
library(psych)

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
