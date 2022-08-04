library(quantmod)
library(stats)

coins <- c("BTC-USD", "BNB-USD", "ETH-USD", "XRP-USD", "LTC-USD", "ADA-USD", "DOGE-USD")

##getSymbols(coins, from = "2018-01-01", to = "2021-10-31", src = "yahoo")
getSymbols(coins, from = "2019-03-15", to = "2021-12-04", src = "yahoo")

df = na.omit(data.frame(diff(log(`BTC-USD`[,6]))[-1], diff(log(`BNB-USD`[,6]))[-1], 
                diff(log(`ETH-USD`[,6]))[-1], diff(log(`XRP-USD`[,6]))[-1],
                diff(log(`LTC-USD`[,6]))[-1], diff(log(`BTC-USD`[,6]))[-1]))
colnames(df) <- c("BTC", "BNB", "ETH", "XRP", "LTC", "DOGE")

length(df$LTC)
head(df); 


factors <- c('DTWEXBGS', 'T10Y2Y')
getSymbols.FRED(factors, from= "2018-01-01", to = "2021-12-31", env = globalenv())
getSymbols('SPY', from= "2018-01-01", to = "2021-12-14", env = globalenv())
getSymbols('^VIX', from= "2018-01-01", to = "2021-12-14", env = globalenv())

length(SPY$SPY.Adjusted)
length(VIX)

VIX <- na.omit(VIX$VIX.Adjusted)
SPY <- na.omit(SPY$SPY.Adjusted)
dollar <- na.omit(DTWEXBGS[3131:4175])
spread <- na.omit(T10Y2Y[10850:11893])

VIX <- diff(log(VIX))[-1]
SPY <- diff(log(SPY))[-1]
dollar <- diff(log(dollar))[-1]
spread <- diff(log(spread))[-1]

length(dollar)
length(spread)
length(VIX)
length(SPY)
f <- data.frame(dollar, spread, SPY, VIX)

all <- data.frame(na.omit(diff(log(`BTC-USD`[,6])))[-1], na.omit(diff(log(`BNB-USD`[,6])))[-1], 
                  na.omit(diff(log(`ETH-USD`[,6])))[-1], na.omit(diff(log(`XRP-USD`[,6])))[-1],
                  na.omit(diff(log(`LTC-USD`[,6])))[-1], na.omit(dollar), na.omit(spread), na.omit(SPY), na.omit(VIX))


km1 = kmeans(t(df),3,nstart=1)
km1$cluster
sort(km1$cluster)

km2 = kmeans(t(all),4,nstart=2)
km2$cluster
sort(km2$cluster)

#Visualization
#install.packages("factoextra")
#install.packages("ggpubr")
library(factoextra)
library(ggpubr)

fviz_cluster(km1, data = t(df),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "text",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
