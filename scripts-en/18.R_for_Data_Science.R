# Chapter 18 Assignment
rm(list=ls())
library(tidyverse)
library(nycflights13)

# 18.1 Merge planes data into flights
data(planes)
data(flights)
f18 <- left_join(flights, planes, by="tailnum")

# 18.2 Use pipe operator to merge multiple datasets into flights
data("weather")
data(airlines)
data(airports)
f19 <- flights %>%
  left_join(weather) %>%
  left_join(airlines) %>%
  left_join(planes) %>%
  left_join(airports, by=c("origin"="faa"))
