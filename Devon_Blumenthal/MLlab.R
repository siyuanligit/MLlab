## Load the data
orders = read.csv("data/orders.csv",header = T,stringsAsFactors = F)
returns = read.csv("data/returns.csv",header = T,stringsAsFactors = F)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

returns = returns %>%
  select(.,-Region)

## Problem 1
orders$Sales = as.numeric(gsub('[$,]','',orders$Sales))
orders$Profit = as.numeric(gsub('[$,]','',orders$Profit))

str(orders)
orders$Order.Date = mdy(orders$Order.Date)
head(orders)
orders$Month = month(orders$Order.Date,label = T)

group_by_month = orders %>%
  group_by(.,Month) %>%
  summarise(.,Count = sum(Quantity))
group_by_month

month_plot = ggplot(data = group_by_month,aes(x = Month, y = Count)) + geom_bar(stat = "identity",aes(fill = Month))
month_plot

group_by_month_category = orders %>%
  group_by(.,Month,Category) %>%
  summarise(.,Count = sum(Quantity))

group_by_month_category

month_category_plot = ggplot(data = group_by_month_category,aes(x = Month, y = Count)) + 
  geom_bar(stat = "identity",aes(fill = Month)) +
  facet_grid(Category ~ .)

month_category_plot

head(orders)

bigdat = right_join(returns,orders,by = "Order.ID") %>%
  mutate(.,Returned = replace_na(Returned,"No"))
str(bigdat)
head(bigdat)

bigdat$Year = as.character(year(bigdat$Order.Date))

group_by_year = bigdat %>%
  group_by(Year) %>%
  summarise(.,Profit_Loss = sum(Profit[Returned == "Yes"]))

group_by_year

profit_loss_plot = ggplot(data = group_by_year,aes(x = Year,y = Profit_Loss)) + 
  geom_col(aes(fill = Year))

group_by_return_customer = bigdat %>%
  filter(.,Returned == "Yes") %>%
  group_by(.,Customer.ID) %>%
  summarise(.,Count = n())

group_by_return_customer %>%
  filter(.,Count > 1) %>%
  nrow()

group_by_return_customer %>%
  filter(.,Count > 5) %>%
  nrow()

group_by_region = bigdat %>%
  group_by(.,Region) %>%
  summarise(.,Percent_Returned = length(unique(Order.ID[Returned == "Yes"]))/length(unique(Order.ID))*100)

region_plot = ggplot(group_by_region,aes(x = reorder(Region,Percent_Returned),y = Percent_Returned)) + 
  geom_col(aes(fill = Region)) +coord_flip()

group_by_sub = bigdat %>%
  group_by(.,Sub.Category) %>%
  summarise(.,Percent_Returned = NROW(Returned[Returned == "Yes"])/NROW(Returned)*100)

sub_plot = ggplot(group_by_sub,aes(x = reorder(Sub.Category,Percent_Returned),y = Percent_Returned)) +
  geom_col(aes(fill = Sub.Category)) + coord_flip()
