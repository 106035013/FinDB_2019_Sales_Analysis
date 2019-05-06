rm(list=ls())


day.price = read.table("D:/gitest/gitest01/0506/price_2010_2018_day.txt", fileEncoding = 'UTF-8')
day.price <- day.price[,-2]
colnames(day.price) <- c("id","", "date", "price")

library(data.table)
dayprice.reorder = dcast(day.price,date~id)
dim(dayprice.reorder)
head(dayprice.reorder)
write_rds(dayprice.reorder, "dayprice.reorder.rds")
