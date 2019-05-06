rm(list=ls())


day.price = read.table("D:/gittest/01/0506/price_2010_2018_day.txt")
day.price <- day.price[,-2]
colnames(day.price) <- c("id", "", "",  "date", "price")
head(day.price)

library(data.table)
dayprice.reorder = dcast(day.price,date~id)
dim(dayprice.reorder)
head(dayprice.reorder)
write_rds(dayprice.reorder, "dayprice.reorder.rds")
