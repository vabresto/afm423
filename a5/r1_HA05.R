################### AFM 423 - Winter 2021 ###################
################### HA05 ####################################
# load libraries
install.packages("sn")
install.packages("PerformanceAnalytics")
install.packages("car")
install.packages("tseries")
install.packages("forecast")
library(sn)
library(PerformanceAnalytics)
library(car)
library(tseries)
library(forecast)
library(quantmod)
library(rugarch)
library(FinTS)

# setwd("~/AFM423/HA05")

#============ Question 1 ============#
options(digits=4)

# download data
symbol.vec = c("^GSPC")
getSymbols(symbol.vec, from ="2000-01-03", to = "2015-10-16")

# extract adjusted closing prices
GSPC = GSPC[, "GSPC.Adjusted", drop=F]

# calculate log-returns for GARCH analysis
GSPC.ret = CalculateReturns(GSPC, method="log")

# remove first NA observation
GSPC.ret = GSPC.ret[-1,]
colnames(GSPC.ret) = "GSPC"

#============ Question ============#

#==== i) ===#
arch5.spec = ugarchspec(variance.model = list(garchOrder=c(5,0)), 
                         mean.model = list(armaOrder=c(0,0)))

GSPC.arch5.fit = ugarchfit(spec = arch5.spec,
                           data = GSPC.ret,
                           solver = "gosolnp", #Random Initialization And Multiple Restarts Of The Solnp Solver
                           solver.control=list(trace = 1)) 
sum(coef(GSPC.arch5.fit)[-c(1,2)])

#==== ii) ===#
par(mfrow=c(2,1))
# GSPC
# sigma(t) = conditional volatility
dataToPlot = cbind(sigma(GSPC.arch5.fit),abs(GSPC.ret))
colnames(dataToPlot) = c("sigma(t)","abs(r_t)")
plot.zoo(dataToPlot ,main="GSPC daily returns", col="blue")

# unconditional variance:
sqrt(uncvariance(GSPC.arch5.fit))
sd(GSPC.ret)


#==== iii) ===#

# specify GARCH(1,1) model with only constant in mean equation
garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(0,0)))
GSPC.garch11.fit = ugarchfit(spec = garch11.spec,
                           data = GSPC.ret,
                           solver = "gosolnp", #Random Initialization And Multiple Restarts Of The Solnp Solver
                           solver.control=list(trace = 1)) 
#Sum of coefficients
sum(coef(GSPC.garch11.fit)[-c(1,2)])

#plot 
dataToPlot = cbind(sigma(GSPC.garch11.fit),abs(GSPC.ret))
colnames(dataToPlot) = c("sigma(t)","abs(r_t)")
plot.zoo(dataToPlot ,main="GSPC daily returns", col="blue")

# compare arch(5) to garch(1,1)
par(mfrow=c(2,1))
dataToPlot = cbind(sigma(GSPC.garch11.fit),sigma(GSPC.arch5.fit))
colnames(dataToPlot) = c("GARCH(1,1)","ARCH(5)")
plot.zoo(dataToPlot ,main="GSPC conditional vol", col="blue")

par(mfrow=c(1,1))

# unconditional variance:
sqrt(uncvariance(GSPC.garch11.fit))





