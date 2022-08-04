# Import the data
library(quantmod)
library(vars)
library(urca)
library(forecast)
library(tidyverse)
library(tidyr)
library(magrittr)
library("readr")

coins <- c("BTC-USD", "BNB-USD", "ETH-USD", "XRP-USD", "LTC-USD", "DOGE-USD", "SPY", "^TNX", "^VIX", "UUP", "USO")
getSymbols(coins, from = "2018-01-01", to = "2021-12-31", src = "yahoo")
btc_ret = as.numeric(na.omit(diff(log(`BTC-USD`[,6]))))
bnb_ret = as.numeric(na.omit(diff(log(`BNB-USD`[,6]))))
eth_ret = as.numeric(na.omit(diff(log(`ETH-USD`[,6]))))
xrp_ret = as.numeric(na.omit(diff(log(`XRP-USD`[,6]))))
ltc_ret = as.numeric(na.omit(diff(log(`LTC-USD`[,6]))))
doge_ret = as.numeric(na.omit(diff(log(`DOGE-USD`[,6]))))
vix_ret = as.numeric(na.omit(diff(log(`VIX`[,6]))))
spy_ret = as.numeric(na.omit(diff(log(`SPY`[,6]))))
tnx_ret = as.numeric(na.omit(diff(log(`TNX`[,6]))))
uup_ret = as.numeric(na.omit(diff(log(`UUP`[,6]))))
uso_ret = as.numeric(na.omit(diff(log(`USO`[,6]))))

df <- data.frame(cbind(btc_ret,bnb_ret,eth_ret,xrp_ret,ltc_ret,doge_ret,vix_ret,spy_ret,tnx_ret,uup_ret,uso_ret))

#first_column <- c("date")
#TNX <- data.frame(first_column)
#TNX %>%
#complete(Date = seq.Date("2018-01-01", "2021-12-31", by="day"))


#df = data.frame(diff(log(`BTC-USD`[,6]))[-1], diff(log(`BNB-USD`[,6]))[-1], 
#                diff(log(`ETH-USD`[,6]))[-1], diff(log(`XRP-USD`[,6]))[-1],
 #               diff(log(`LTC-USD`[,6]))[-1], diff(log(`DOGE-USD`[,6]))[-1] )
#df_side = data.frame(`SPY`[,6][-1], `VIX`[,6][-1])
#df_side$date = rownames(df_side)
#df_extra = as.data.frame(`TNX`[,6])
#df_extra$date = rownames(df_extra)
#df_side = left_join(df_side, df_extra, by = "date")


# Variable declaration
BTC_USD <- ts(df$btc_ret, start = c(2018,1,1), frequency = 365)
BNB_USD <- ts(df$bnb_ret, start = c(2018,1,1), frequency = 365)
ETH_USD <- ts(df$eth_ret, start = c(2018,1,1), frequency = 365)
XRP_USD <- ts(df$xrp_ret, start = c(2018,1,1), frequency = 365)
LTC_USD <- ts(df$ltc_ret, start = c(2018,1,1), frequency = 365)
DOGE_USD <- ts(df$doge_ret, start = c(2018,1,1), frequency = 365)


#Bind into a system
###dset <- data.frame(BTC_USD,BNB_USD,ETH_USD,XRP_USD,LTC_USD,DOGE_USD, SPY, VIX)

#Lag Selection Criteria
#FPE
lagselect <- VARselect(df, type = "const")
lagselect$selection
#Since 5 was chosen, we use 5 - 1 = 4


#Johansen Testing (Trace)
ctest1t <- ca.jo(df, type = "trace", ecdet = "const", K = 3)
summary(ctest1t)
#pass

#Johansen Testing (MaxEigen)
ctest1e <- ca.jo(df, type = "eigen", ecdet = "const", K = 3)
summary(ctest1e)
#pass

#PAIRS TESTING
dset2 <- data.frame(BTC_USD,BNB_USD)
ctest2 <- ca.jo(dset2, type = "trace", ecdet = "const", K = 4)
summary(ctest2)
#pass

dset3 <- data.frame(BTC_USD, ETH_USD)
ctest3 <- ca.jo(dset3, type = "trace", ecdet = "const", K = 4)
summary(ctest3)
#pass

dset4 <- data.frame(BTC_USD, XRP_USD)
ctest4 <- ca.jo(dset4, type = "trace", ecdet = "const", K = 4)
summary(ctest4)
#pass

dset5 <- data.frame(BTC_USD, LTC_USD)
ctest5 <- ca.jo(dset5, type = "trace", ecdet = "const", K = 4)
summary(ctest5)
#pass

dset6 <- data.frame(BNB_USD, ETH_USD)
ctest6 <- ca.jo(dset6, type = "trace", ecdet = "const", K = 4)
summary(ctest6)
#pass

dset7 <- data.frame(BNB_USD, XRP_USD)
ctest7 <- ca.jo(dset7, type = "trace", ecdet = "const", K = 4)
summary(ctest7)
#pass

dset8 <- data.frame(BNB_USD, LTC_USD)
ctest8 <- ca.jo(dset8, type = "trace", ecdet = "const", K = 4)
summary(ctest8)
#pass

dset9 <- data.frame(ETH_USD, XRP_USD)
ctest9<- ca.jo(dset9, type = "trace", ecdet = "const", K = 4)
summary(ctest9)
#pass

dset10 <- data.frame(ETH_USD, LTC_USD)
ctest10 <- ca.jo(dset10, type = "trace", ecdet = "const", K = 4)
summary(ctest10)
#pass

dset11 <- data.frame(XRP_USD, LTC_USD)
ctest10 <- ca.jo(dset10, type = "trace", ecdet = "const", K = 4)
summary(ctest10)
#pass

dset12 <- data.frame(DOGE_USD,BNB_USD)
ctest12 <- ca.jo(dset12, type = "trace", ecdet = "const", K = 4)
summary(ctest2)
#pass

dset13 <- data.frame(DOGE_USD, ETH_USD)
ctest13 <- ca.jo(dset13, type = "trace", ecdet = "const", K = 4)
summary(ctest3)
#pass

dset14 <- data.frame(DOGE_USD, XRP_USD)
ctest14 <- ca.jo(dset14, type = "trace", ecdet = "const", K = 4)
summary(ctest4)
#pass

dset15 <- data.frame(DOGE_USD, LTC_USD)
ctest15 <- ca.jo(dset15, type = "trace", ecdet = "const", K = 4)
summary(ctest5)
#pass

dset16 <- data.frame(DOGE_USD, BTC_USD)
ctest16 <- ca.jo(dset16, type = "trace", ecdet = "const", K = 4)
summary(ctest5)
#pass

