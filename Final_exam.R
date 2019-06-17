rm(list=ls())

#1.使用套件tidyquant, timetk，並讀入資料tej_day_price_2017_2018。


library(tidyquant)
library(timetk)

stock_day_2_year<-read_tsv("D:/gitest/gitest01/0617/data_wrangle_practice/tej_day_price_2017_2018.txt")

glimpse(stock_day_2_year)

#2.選取欄位“證券代碼”, “簡稱”, “年月日”, “收盤價(元)”, “市值(百萬元)”, 並將名稱改為“id”, “name”, “date”, “price”, “cap”。

price_day_2_year <- stock_day_2_year %>% 
                    rename(id    = 證券代碼, 
                           name  = 簡稱, 
                           date  = 年月日, 
                           price = `收盤價(元)`,
                           cap   = `市值(百萬元)`
                           ) 

dim(price_day_2_year)

#3.選取id, date, price, 並將id改為文字格式，date改為日期格式，並將資料格式改為寬資料。

price_day_2_year_1 <- stock_day_2_year %>% 
                    rename(id    = 證券代碼, 
                           name  = 簡稱, 
                           date  = 年月日, 
                           price = `收盤價(元)`,
                           cap   = `市值(百萬元)`
                           ) %>% 
                    mutate(id = as.character(id)) %>%
                    mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
                    select(id, date, price) %>% 
                    spread(key = id, value = price) 

dim(price_day_2_year_1)

#4.檢查含有NA的股票代碼及其NA的個數。

price_day_2_year_na <- price_day_2_year_1 %>% 
                       map_df(~sum(is.na(.))) %>% 
                       gather() %>% 
                       filter(value!=0)
price_day_2_year_na

price_day_2_year_na.1 <- price_day_2_year_1 %>% 
                         # last observation carried forward
                         map_df(~sum(is.na(.))) %>% 
                         gather() %>% 
                         filter(value!=0)

price_day_2_year_na.1

#5.將NA值以最近的股價取代。

price_day_2_year_clear <-  price_day_2_year_1%>% 
                           na.locf(fromLast = TRUE, na.rm=FALSE) %>%
                           select(-c("2025", "6131"))

#6.刪除上題中仍含有NA值的股票, 並確認股票數量及筆數。

dim(price_day_2_year_clear)

#7.將資料轉為xts, 計算日報酬率(以log計算), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。

ret_day_2_year <- price_day_2_year_clear %>% 
                  select(1:6) %>% 
                  tk_xts(select = -date, date_var = date) %>% 
                  Return.calculate(method = "log") %>%
                  na.omit()

dim(ret_day_2_year)

head(ret_day_2_year, 5)

#8.計算月報酬率(以log計算), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。

price_day_2_year.xts <- price_day_2_year_clear %>%
                        select(1:6) %>%
                        tk_xts(select = -date, date_var = date)  

ret_mon_2_year.xts <- price_day_2_year.xts %>% 
                       to.period(period = "months", 
                                 indexAt = "lastof", 
                                 OHLC= FALSE) %>% 
                       Return.calculate(method = "log") %>%
                       na.omit()


dim(ret_mon_2_year.xts)

head(ret_mon_2_year.xts, 5)

#9.找出2017及2018年年底市值最大的前20家公司代碼, 簡稱, 並修改資本額格式，計算每家公司市值佔20家總市值的百分比。

tej <- read_tsv("D:/gitest/gitest01/0617/data_wrangle_practice/tej_day_price_2017_2018.txt", col_names = TRUE)

glimpse(tej)

tej1<-tej %>% select('證券代碼', '簡稱', '年月日', '市值(百萬元)') %>% 
  rename(id = '證券代碼', name = '簡稱',  date = '年月日', cap = '市值(百萬元)') %>%      
  mutate(date = date %>% as.character %>% as.Date('%Y%m%d')) %>% 
  mutate(id = id %>% as.character) %>% 
  arrange( desc(date), desc(cap)) %>% 
  select(3,4,1,2) 

  
 filter(order_date %>% between(left = ymd("2017-12-29"), right = ymd("2018-12-28")))


mutate(diff_1 = sales - sales_lag_1) %>%
  mutate(pct_diff_1 = diff_1 / sales_lag_1) %>%
  mutate(pct_diff_1_chr = scales::percent(pct_diff_1))

#11.將題7的日報酬格式由寬格式改為長格式(如下),並只選取2018年的資料。

ret_day_2_year.xts <- price_day_2_year_clear %>% 
                      tk_xts(select = -date, date_var = date) %>% 
                      Return.calculate(method = "log") %>%
                      na.omit()

ret_day_2_year_1 <- tk_tbl(ret_day_2_year.xts)

gather(ret_day_2_year_1)


spread(key = id, value = price) 
