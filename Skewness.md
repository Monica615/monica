install.packages('DBI')
install.packages('RMySQL')
library(DBI)
library(RMySQL)

install.packages("tidyverse")
install.packages("PerformanceAnalytics")
install.packages("quantmod")
install.packages("scales")
install.packages("timetk")
install.packages("tidyquant")
install.packages("tibbletime")
install.packages("highcharter")
install.packages("readxl")
install.packages("xts")

library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(scales)
library(timetk)
library(tidyquant)
library(tibbletime)
library(highcharter)
library(readxl)
library(xts)  

install.packages("data.table")
library(data.table)

#导入股票指数——上证指数000001，命名为ssec
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `index_code`,`start_date`,  `close`
FROM `cn_stock_index`.`monthly_quote`
WHERE index_code='000001' 
ORDER BY `start_date` "
dbGetQuery(mydb,SQL_statement)
ssec <- dbGetQuery(mydb,SQL_statement)
