library(quantmod)
library(stats)
library(dplyr)

coins <- c("BTC-USD", "BNB-USD", "ETH-USD", "XRP-USD", "LTC-USD", "ADA-USD", "DOGE-USD")

##getSymbols(coins, from = "2018-01-01", to = "2021-10-31", src = "yahoo")
getSymbols(coins, from = "2018-01-01", to = "2021-12-31", src = "yahoo")
`BTC-USD`
return_btc=diff(log(`BTC-USD`))[-1]
return_btc = periodReturn(`BTC-USD`,period="monthly",type="log")
return_btc
length(return_btc)
return_BNB=periodReturn(`BNB-USD`,period="monthly",type="log")
return_ETH=periodReturn(`ETH-USD`,period="monthly",type="log")
return_XRP=periodReturn(`XRP-USD`,period="monthly",type="log")
return_LTC=periodReturn(`LTC-USD`,period="monthly",type="log")
return_ADA=periodReturn(`ADA-USD`,period="monthly",type="log")
return_DOGE=periodReturn(`DOGE-USD`,period="monthly",type="log")


sidefactors <- c('SPY','^VIX','IEF',"DWAS","VV","UUP","GLD","IAU","USO")
getSymbols(sidefactors, from= "2018-01-01", to = "2021-12-31", src = "yahoo")
spy_return = periodReturn(SPY,period="monthly",type="log")
VIX_return = periodReturn(VIX,period="monthly",type="log")
IEF_return = periodReturn(IEF,period="monthly",type="log")
DWAS_return = periodReturn(DWAS,period="monthly",type="log")
VV_return = periodReturn(VV,period="monthly",type="log")
UUP_return =periodReturn(UUP,period="monthly",type="log")
GLD_return =periodReturn(GLD,period="monthly",type="log")
IAU_return =periodReturn(IAU,period="monthly",type="log")
USO_return =periodReturn(USO,period="monthly",type="log")
COINS<- data.frame(return_btc,return_BNB,return_ADA,return_DOGE,return_ETH,return_XRP,
                   return_LTC,return_XRP)
colnames(COINS) <- c("BTC", "BNB", "ADA","DOGE",'ETH', "XRP", "LTC", "XRP")
all<- data.frame(return_btc,return_BNB,return_ADA,return_DOGE,return_ETH,return_XRP,
                 return_LTC,return_XRP,spy_return,VIX_return,IEF_return,DWAS_return,VV_return,
                 UUP_return,GLD_return,IAU_return,USO_return)
colnames(all) <- c("BTC", "BNB", "ADA","DOGE",'ETH', "XRP", "LTC", "XRP","SPY",
                   "VIX","IEF","DWAS","VV","UUP","GLD","IAU","USO")
cor(all)
km1 = kmeans(t(COINS),3,nstart=1)
km1$cluster
sort(km1$cluster)
#"SPY","VIX","IEF","DWAS","VV","UUP")
#DWAS  tracks an index of 200 small-cap securities with the
#best relative strength indicators.
#VV. Vanguard Large-Cap Index Fund ETF Shares
#uup track the dollar index
#GLD,IAU are the gold index
#USO is the oil index




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
