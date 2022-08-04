library(quantmod)
library(PerformanceAnalytics)
library(fBasics)
set.seed(0)

#Load Data
coins <- c("BTC-USD", "BNB-USD", "ETH-USD", "XRP-USD", "LTC-USD", "DOGE-USD")
getSymbols(coins,from = "2018-01-01",to="2021-12-31",src="yahoo")
prices <- na.omit(merge(`BTC-USD`[,6],`BNB-USD`[,6],`ETH-USD`[,6],`XRP-USD`[,6],`LTC-USD`[,6],
                        `DOGE-USD`[,6]))
colnames(prices) <- c("BTC Prices","BNB Price","ETH Price","XRP Price", "LTC Price", "DOGE Price")
testing_prices <- prices["2021-07-01/2021-12-31"]
training_prices <- prices["/2021-06-30"]
head(prices)

#Returns
returns <- na.omit(merge(diff(log(`BTC-USD`[,6])),diff(log(`BNB-USD`[,6])),diff(log(`ETH-USD`[,6])
),
diff(log(`XRP-USD`[,6])),diff(log(`LTC-USD`[,6])), diff(log(`DOGE-USD`[,6]))))
colnames(returns) <- c("BTC Return","BNB Return","ETH Return","XRP Return", "LTC Return",
                       "DOGE Return")
testing_return <- returns["2021-07-01/2021-12-31"]
training_returns <- returns["2018-01-01/2021-06-30"]
head(returns)

# BTC
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`BTC-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`BTC-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,1]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",1]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,1])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,1]), type = "l", col="red",ylim =c(18000,90000), 
     main = "Actual Price Movements of BTC verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price of BTC")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 90000, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
## Test for normality
temp_r <- na.omit(as.numeric(diff(log(`BTC-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)

# BNB
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`BNB-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`BNB-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,2]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",2]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,2])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,2]), type = "l", col="red",ylim =c(0,1800), 
     main = "Actual Price Movements of BNB verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 1600, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
## Test for normality
temp_r <- na.omit(as.numeric(diff(log(`BNB-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)

# ETH
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`ETH-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`ETH-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,3]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",3]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,3])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,3]), type = "l", col="red",ylim =c(0,10000), 
     main = "Actual Price Movements of ETH verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 10000, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
## Test for normality
temp_r <- na.omit(as.numeric(diff(log(`ETH-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)

# XRP
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`XRP-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`XRP-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,4]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",4]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,4])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,4]), type = "l", col="red",ylim =c(0,2.75), 
     main = "Actual Price Movements of XRP verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 2.75, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
## Test for normality
temp_r <- na.omit(as.numeric(diff(log(`XRP-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)

# LTC
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`LTC-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`LTC-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,5]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",5]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,5])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,5]), type = "l", col="red",ylim =c(0,500), 
     main = "Actual Price Movements of LTC verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 500, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
temp_r <- na.omit(as.numeric(diff(log(`LTC-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)

# DOGE
## Step 1) Calibration
set.seed(0)
dt = 1/365
m = mean(na.omit(as.numeric(diff(log(`DOGE-USD`[,6])))))
s = sd(na.omit(as.numeric(diff(log(`DOGE-USD`[,6])))))
sig <- s/sqrt(dt)
mu <- m/dt + (sig^2)/2
# Step 2) Create the GBM Simulation
N <- 10
days <- length(testing_return[,6]) # number of days to simulate ahead
S0 <- as.numeric(training_prices["2021-06-30",6]) # last price of SPY
drift <- mu - (sig^2)/2
sim_Geo_BM <- function(n) {
  R_seq <- rnorm(days,drift*dt,sig*sqrt(dt)) # need to simulate returns
  S <- S0*exp(c(0,cumsum(R_seq))) # take the exp of returns
  return(S)
}
S_mat <- sapply(1:N,sim_Geo_BM)
S_total <- c()
for (i in 1:nrow(S_mat)){
  S_total <- c(S_total,mean(S_mat[i,]))
}
# Step 4) MSE
S_returns = diff(log(S_total))
temp_val = as.numeric(testing_return[,6])
mean_sq <- c()
for (i in 1:184){
  mean_sq <- c(mean_sq, (temp_val[i] - S_returns[i])^2)
}
(1/184) * sum(mean_sq)
#Plots
plot(as.numeric(testing_prices[,6]), type = "l", col="red",ylim =c(0,1.45), 
     main = "Actual Price Movements of DOGE verse GBM Simulated Price Movements",
     xlab="Simulated Days (07/01/2021 to 12/31/2021)", ylab="Price")
lines(S_mat[,1], col = "black")
lines(S_mat[,2], col = "black")
lines(S_mat[,3], col = "black")
lines(S_mat[,4], col = "black")
lines(S_mat[,5], col = "black")
lines(S_mat[,6], col = "black")
lines(S_mat[,7], col = "black")
lines(S_mat[,8], col = "black")
lines(S_mat[,9], col = "black")
lines(S_mat[,10], col = "black")
lines(S_total, col = "blue")
legend(0, 1.45, legend=c("Actual Price Movements", "GBM Simulated Prices", "GBM Average"),
       col=c("red", "black", "blue"), lty=1, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
temp_r <- na.omit(as.numeric(diff(log(`DOGE-USD`[,6]))))
normalTest(temp_r, method = "jb")
qqnorm(temp_r); qqline(temp_r)