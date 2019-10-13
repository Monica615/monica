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

#导入股票指数——A股指数000002，命名为ssea
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `index_code`,`start_date`,  `close`
FROM `cn_stock_index`.`monthly_quote`
WHERE index_code='000002'
ORDER BY `start_date` "
dbGetQuery(mydb,SQL_statement)
ssea <- dbGetQuery(mydb,SQL_statement)

#导入股票指数——沪深指数000300，命名为hs
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `index_code`,`start_date`,  `close`
FROM `cn_stock_index`.`monthly_quote`
WHERE index_code='000300'
ORDER BY `start_date` "
dbGetQuery(mydb,SQL_statement)
hs <- dbGetQuery(mydb,SQL_statement)

#导入股票指数——创业板指数399012，命名为cyb
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `index_code`,`start_date`,  `close`
FROM `cn_stock_index`.`monthly_quote`
WHERE index_code='399012'
ORDER BY `start_date` "
dbGetQuery(mydb,SQL_statement)
cyb <- dbGetQuery(mydb,SQL_statement)

#导入股票指数——深圳成指399001，命名为szi
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `index_code`,`start_date`,  `close`
FROM `cn_stock_index`.`monthly_quote`
WHERE index_code='399001'
ORDER BY `start_date` "
dbGetQuery(mydb,SQL_statement)
szi<- dbGetQuery(mydb,SQL_statement)

#截取2013年到2018年每年月度数据（72个）
szi1<-as.data.table(szi)
szi2<-szi1[262:333]

ssec1<-as.data.table(ssec)
ssec2<-ssec1[266:337]

ssea1<-as.data.table(ssea)
ssea2<-ssea1[266:337]

hs1<-as.data.table(hs)  
hs2<-hs1[98:169]

cyb1<-as.data.table(cyb)
cyb2<-cyb1[1:72]

#合并五个指数数据
data1=ssec2[ssea2,on="start_date"]
data2=data1[hs2,on="start_date"]
data3=data2[szi2,on="start_date"]
data4=data3[cyb2,on="start_date"]
data5=data4[,!c(1,4,6,8,10)]
setnames(data5,c("close","i.close","i.close.1","i.close.2","i.close.3"),c("ssec","ssea","hs","szi","cyb"))
prices_monthly<-data5

#把合并后的数据转化成时间序列数据
data6=prices_monthly[,start_date]
data7=as.Date(data6)
prices_monthly[,':='(date1=data7)]
prices_monthly=prices_monthly[,!1]
setcolorder(prices_monthly,c("date1","ssec","hs","ssea","szi","cyb"))
prices_monthly_xts<-as.xts.data.table(prices_monthly)

  

#第一种方法算偏度（xts）
  asset_returns_xts<-
      Return.calculate(prices_monthly_xts,
                       method="log") %>%
    na.omit()
  head(asset_returns_xts,3)
  
  
  
  w<-c(0.2,
       0.25,
       0.2,
       0.25,
       0.1)

  #资产池回报率计算
  portfolio_returns_xts_rebalanced_monthly<-
    Return.portfolio(asset_returns_xts,
                     weight=w,
                     rebalance_on = "month")  %>%
    'colnames<-'("returns")
  head(portfolio_returns_xts_rebalanced_monthly,3)

  #计算投资组合资产池偏度
  skew_xts<-
    skewness(portfolio_returns_xts_rebalanced_monthly$returns)
  skew_xts
  
#第二种方法算偏度（tidyverse）
  window<-24
  rolling_skew_xts<-
    rollapply(portfolio_returns_xts_rebalanced_monthly,
              FUN=skewness,
              width=window) %>%
    na.omit()
  
  
  #计算收益率：
  asset_returns_dplyr_byhand<-
    prices_monthly_xts %>% 
    # convert the index to a date 
    data.frame(date = index(.)) %>% 
    # now remove the index because it got converted to row names remove_rownames() %>%
    gather(asset, prices, -date) %>% 
    group_by(asset) %>% 
    mutate(returns = (log(prices) - log(lag(prices)))) %>% 
    select(-prices) %>% 
    spread(asset, returns)
    
    asset_returns_dplyr_byhand<-
    asset_returns_dplyr_byhand %>%
    na.omit()
  
asset_returns_long<-
  asset_returns_dplyr_byhand%>%
  gather(asset,returns,-date) %>%
  group_by(asset)
head(asset_returns_long,3)


#转化成tq格式：
portfolio_returns_tq_rebalanced_monthly<-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset, 
               returns_col = returns, 
               weights = w, 
               col_rename = "returns", 
               rebalance_on = "months")


skew_tidy<-
  portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(skew_builtin = skewness(returns), 
            skew_byhand = (sum((returns - mean(returns))^3)/length(returns))/ 
              ((sum((returns - mean(returns))^2)/length(returns)))^(3/2)) %>% 
  select(skew_builtin, skew_byhand)
skew_tidy %>% 
  mutate(xts = coredata(skew_xts)) %>%
  mutate_all(funs(round(., 3)))

#数据可视化
#1.可视化偏度
portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) + 
  geom_histogram(alpha = .7, 
                 binwidth = .003, 
                 fill = "cornflowerblue", 
                 color = "cornflowerblue") + 
  scale_x_continuous(breaks = 
                       pretty_breaks(n = 10))


#2.数据处理：给低于平均值两个标准差的数据标红
portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(hist_col_red = if_else(returns < (mean(returns) - 2*sd(returns)), 
                                returns, as.numeric(NA)), 
         returns =
           if_else(returns > (mean(returns) - 2*sd(returns)), 
                   returns, as.numeric(NA))) %>% 
  ggplot() + 
  geom_histogram(aes(x = hist_col_red),
              alpha = .7, 
              binwidth = .003, 
              fill = "red", 
              color = "red") + 
  geom_histogram(aes(x = returns), 
                 alpha = .7, 
                 binwidth = .003, 
                 fill = "cornflowerblue", 
                 color = "cornflowerblue") + 
  scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
  xlab("monthly returns")


#3.建立偏度密度图
portfolio_density_plot<-
  portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) + 
  stat_density(geom = "line", 
               alpha = 1,
               colour = "cornflowerblue")
portfolio_density_plot


shaded_area_data<-
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x <
           mean(portfolio_returns_tq_rebalanced_monthly$returns))
portfolio_density_plot_shaded<-
  portfolio_density_plot + 
  geom_area(data = shaded_area_data,
            aes(x = x, y = y), 
            fill="pink", 
            alpha = 0.5)
portfolio_density_plot_shaded


#4.比较负偏度下平均值和中位数大小
median <-
  median(portfolio_returns_tq_rebalanced_monthly$returns) 
mean <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

median_line_data<-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <= median)

portfolio_density_plot_shaded +
  geom_segment(data = shaded_area_data, 
               aes(x = mean, 
                   y = 0, 
                   xend = mean,
                   yend = density), 
               color = "red", 
               linetype = "dotted") +
  annotate(geom = "text", 
           x = mean, y = 5,
           label = "mean",
           color = "red", 
           fontface = "plain", angle = 90,
           alpha = .8, 
           vjust = -1.75) +
  
  geom_segment(data = median_line_data, 
               aes(x = median, 
                   y = 0, 
                   xend = median, 
                   yend = density),
               color = "black",
               linetype = "dotted") +
  annotate(geom = "text", 
           x = median, 
           y = 5, 
           label = "median",
           fontface = "plain", 
           angle = 90, 
           alpha = .8, 
           vjust = 1.75) + 
  ggtitle("Density Plot Illustrating Skewness")


#5.将投资组合偏度与原来5种指数的偏度对比
asset_returns_long %>% 
  summarize(skew_assets = skewness(returns)) %>% 
  add_row(asset = "Portfolio",
          skew_assets = skew_tidy$skew_byhand)%>%
  ggplot(aes(x = asset, 
             y = skew_assets, 
             colour = asset)) + 
  geom_point() + 
  geom_text(
    aes(x = "Portfolio", 
        y =
          skew_tidy$skew_builtin + .04), 
    label = "Portfolio", 
    color = "cornflowerblue") + 
  labs(y = "skewness")



#滚动偏度（rolling skewness）
#1、第一种方法——xts
window <-24
rolling_skew_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = skewness,
            width = window) %>%
  na.omit()


#2、第二种方法——tidyverse with tibbletime
skew_roll_24 <-
  rollify(skewness, window = window)

roll_skew_tibbletime<-
  portfolio_returns_tq_rebalanced_monthly %>% 
  as_tbl_time(index = date) %>% 
  mutate(skew = skew_roll_24(returns)) %>% 
  select(-returns) %>%
  na.omit()


#3、第三种方法——tidyquant
rolling_skew_tq<-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(select = returns, 
            mutate_fun = rollapply, 
            width = window, 
            FUN = skewness, 
            col_rename = "tq") %>% 
  na.omit()


#4、数据可视化：表格呈现
rolling_skew_tq %>% 
  select(-returns) %>% 
  mutate(xts = coredata(rolling_skew_xts), 
         tbltime = roll_skew_tibbletime$skew) %>%
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  tail(3)


#5、数据可视化：图表呈现——设置波动范围在3到-3之间
highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month Skewness") %>% 
  hc_add_series(rolling_skew_xts, 
                name = "Rolling skewness", 
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "skewness"), 
           opposite = FALSE,
           max = 1, 
           min = -1) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)


#6、数据可视化：图表呈现——缩放y轴，调整y轴后效果更明显
rolling_skew_tq %>% 
  ggplot(aes(x = date, y = tq)) + 
  geom_line(color = "cornflowerblue") + 
  ggtitle("Rolling 24-Month Skew ") +
  ylab(paste("Rolling ", window, " month skewness", sep = " ")) + 
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = pretty_breaks(n = 8)) + 
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme_update(plot.title = element_text(hjust = 0.5))
