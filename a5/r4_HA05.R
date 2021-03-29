################### AFM 423 - Winter 2021 ###################
################### HA#05 ###################################

# load libraries

library(sn)
library(PerformanceAnalytics)
library(car)
library(tseries)
library(forecast)
library(quantmod)
library(rugarch)
library(FinTS)


#============ Question 4 ============#
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

#====================================#

actfun = function(x){x}
sigmoid = function(x){exp(x)/(1+exp(x))}
lstm.fwdprop = function (x,w){
  omega = w[1]; alpha = w[2]; beta = w[3]
  wf = w[4]; af = w[5]; bf = w[6]
  wi = w[7]; ai = w[8]; bi = w[9]
  wo = w[10]; ao = w[11]; bo = w[12]
  
  sigma2t = rep(mean(x),nrow(x))
  ot = rep(0, nrow(x))
  ft = rep(0, nrow(x))
  it = rep(0, nrow(x))
  Ct = rep(0, nrow(x))
  
  for (k in 2:nrow(x)){
    ot[k] = sigmoid(wo + ao * x[k] + bo * sigma2t[k-1])
    ft[k] = sigmoid(wf + af * x[k] + bf * sigma2t[k-1])
    it[k] = sigmoid(wi + ai * x[k] + bi * sigma2t[k-1])
    Ct[k] = ft[k]*Ct[k-1] + it[k]*( omega + alpha*x[k] + beta*sigma2t[k-1])
    sigma2t[k] = ot[k] * Ct[k]
  }
  y = sigma2t # indentity function output
  list (output = y)
}

loss.fun = function (init.w,x,y){
  y.hat = lstm.fwdprop(x,init.w)$output
  return(-sum(log(dnorm(y,0,sqrt(y.hat)))))
}

#========================================================#

nsplit.GSPC = which( time(GSPC.ret[,1]) == "2012-01-03")
GSPC.train = GSPC.ret[1:nsplit.GSPC]
GSPC.test = MSFT.ret[(nsplit.GSPC+1):length(GSPC.ret)]

#=======================================================#

# GARCH(1,1)

myGARCHfit = function(params, x, y){
  omega = params[1]
  alphaL1 = params[2]
  betaL1 = params[3]
  n = length(y)
  
  sigma2t = rep(mean(x),n)
  for (i in 2:n){
    sigma2t[i] = omega + alphaL1 * x[i] + betaL1 * sigma2t[i-1]
  }
  
  sigmat = sqrt(sigma2t)
  
  log.likelihood = -sum(log(dnorm(y, 0, sigmat)))  
  
  return(log.likelihood)
}
init.guess=c(0,rep(0.5,2))
garch = optim(par = init.guess, fn=myGARCHfit,
              x = (GSPC.train^2)[-length(GSPC.train)],
              y = GSPC.train[-1] )
garch
# LSTM
library(Rsolnp)

x = (GSPC.train^2)[-length(GSPC.train)]
y = GSPC.train[-1]

cl = makePSOCKcluster(2)
clusterExport(cl, c("sigmoid","loss.fun","lstm.fwdprop"))
lstm = gosolnp(fun = loss.fun,  x = x, y = y,
               LB = c(0,0,0,rep(c(-5,-1e2,-1e2),3)),
               UB = c(0.05,1,1,rep(c(5,1e2,1e2),3)),
               cluster = cl) 
stopCluster(cl)

#========================================================#
garchfit = function(garch,Lag1){
  beta.val = rep(mean(Lag1),length(Lag1))
  for (i in 2:length(Lag1)){
    beta.val[i] = garch$par[1] + garch$par[2] * Lag1[i] + garch$par[3] * beta.val[i-1]
  }
  
  y.hat.g = garch$par[1] +  
    garch$par[2]*Lag1 + 
    garch$par[3]*beta.val
 return(y.hat.g)
}

y.garch = garchfit(garch = garch, Lag1 = GSPC.ret[-length(GSPC.ret)]^2 )
y.lstm = lstm.fwdprop(x = GSPC.ret[-length(GSPC.ret)]^2, w = lstm$pars )

# Plots
par(mfrow=c(1,1))

plot(as.numeric(unlist(y.lstm)),col="red",lwd=3,type="l",
     ylim=c(0,0.0060),ylab="Return volatility forecasts",
     xlab="Time")
points(as.numeric(y.garch),type="l",lty=2)
legend(x=500,y=0.006,c("GARCH(1,1)","LSTM"),col=c("black","red"),lty=c(2,1))
abline(v=nsplit.GSPC,lwd=2)

#==============================================#

lstm.gates = function (x,w){
  omega = w[1]; alpha = w[2]; beta = w[3]
  wf = w[4]; af = w[5]; bf = w[6]
  wi = w[7]; ai = w[8]; bi = w[9]
  wo = w[10]; ao = w[11]; bo = w[12]
  
  sigma2t = rep(mean(x),nrow(x))
  ot = rep(0, nrow(x))
  ft = rep(0, nrow(x))
  it = rep(0, nrow(x))
  Ct = rep(0, nrow(x))
  
  for (k in 2:nrow(x)){
    ot[k] = sigmoid(wo + ao * x[k] + bo * sigma2t[k-1])
    ft[k] = sigmoid(wf + af * x[k] + bf * sigma2t[k-1])
    it[k] = sigmoid(wi + ai * x[k] + bi * sigma2t[k-1])
    Ct[k] = ft[k]*Ct[k-1] + it[k]*( omega + alpha*x[k] + beta*sigma2t[k-1])
    sigma2t[k] = ot[k] * Ct[k]
  }
  y = sigma2t # indentity function output
  list (gates = cbind(ot,ft,it))
}

GSPC.gates = lstm.gates(x = GSPC.ret[-length(GSPC.ret)]^2, w = lstm$pars )

par(mfrow=c(1,1))
dataToPlot = cbind(GSPC.gates$gates[-1,1],
                   GSPC.gates$gates[-1,2],
                   GSPC.gates$gates[-1,3])
colnames(dataToPlot) = c("ot", "ft", "it")
plot.zoo(dataToPlot, main="GSPC Gates", col="blue", cex.lab=2.5,
         xlab="Time")

#==============================================#
#TEST DATASET
table = matrix(0,nrow=4,ncol=4)
metrics = function(x, y, garch){
  vec = vector(length=4)
  if(garch==TRUE){
    nparam = 3
    }else{
    nparam = 12
  }
  
  
  vec[1] = sum( abs(x-y) ) #SAD
  vec[2] = sum( (x-y)^2 ) #SSE
  vec[3] = 1 - (vec[2]/sum( (y-mean(y))^2 ))
  vec[4] = 2 * ( nparam - sum(log(dnorm(x=y,0, sd = sqrt(x)))) ) # AIC
    
  return(vec)
}

y.lstm.test = lstm.fwdprop(x = (GSPC.test^2)[-length(GSPC.test)], w = lstm$pars )
y.garch.test = garchfit(garch = garch, Lag1 = (GSPC.test^2)[-length(GSPC.test)] )

table[1,]=metrics(x = y.garch.test , y = (GSPC.test^2)[-length(GSPC.test)],garch = TRUE)
table[2,]=metrics(x = y.lstm.test$output , y = (GSPC.test^2)[-length(GSPC.test)],garch = FALSE)

table

