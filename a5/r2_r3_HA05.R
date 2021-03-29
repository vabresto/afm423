################### AFM 423 - Winter 2021 ###################
################### HA05 ####################################
# load libraries

library(sn)
library(PerformanceAnalytics)
library(car)
library(tseries)
library(forecast)
library(quantmod)
library(rugarch)
library(FinTS)

#============ Question 2 ============#
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

#=============================================#
init.guess=c(0,rep(0.2,5))
Lag1 = as.numeric(GSPC.ret[-c(1:4,3971)])^2
Lag2 = as.numeric(GSPC.ret[-c(1:3,3970:3971)])^2
Lag3 = as.numeric(GSPC.ret[-c(1:2,3969:3971)])^2
Lag4 = as.numeric(GSPC.ret[-c(1,3968:3971)])^2
Lag5 = as.numeric(GSPC.ret[-c(3967:3971)])^2

#ARCHfit

myARCHfit = function(params, x, y){
  sigma2t = params[1] + 
    params[2] * x[,1] + 
    params[3] * x[,2] + 
    params[4] * x[,3] + 
    params[5] * x[,4] + 
    params[6] * x[,5]
  sigmat = sqrt(sigma2t)
  log.likelihood = -sum(log(dnorm(y, 0, sigmat)))  
  return(log.likelihood)
}

arch = optim(par = init.guess, fn=myARCHfit,
      x = cbind(Lag1,Lag2,Lag3,Lag4,Lag5), y = as.numeric(GSPC.ret[-c(1:5)]) )

#FNN
actfun = function(x) {x}

fwdprop = function (x, w){
  h = actfun(cbind(1,x)%*%w)
  y = h
  list(output = y)
}

loss.fun = function(init.w, x, y){
  y.hat = fwdprop(x,init.w)$output
  return( -sum( log( dnorm(y,0,sqrt(y.hat)) ) ) )
}

arch.nn = optim(par = init.guess, fn=loss.fun,
                x = cbind(Lag1,Lag2,Lag3,Lag4,Lag5), y = as.numeric(GSPC.ret[-c(1:5)]) )

#compare estimated parameters
arch$par
arch.nn$par

# Plot for a)
y.hatnn = arch.nn$par[1] +  
        arch.nn$par[2]*Lag1 + 
        arch.nn$par[3]*Lag2 +
        arch.nn$par[4]*Lag3 +
        arch.nn$par[5]*Lag4 +
        arch.nn$par[6]*Lag5 
y.hat = arch$par[1] +  
  arch$par[2]*Lag1 + 
  arch$par[3]*Lag2 +
  arch$par[4]*Lag3 +
  arch$par[5]*Lag4 +
  arch$par[6]*Lag5 

par(mfrow=c(1,2))
plot(y = y.hatnn, x = y.hat, ylab="Neural Network", xlab = "ARCH(5)",
     col="blue",main="FNN vs ARCH(5) for GSPC",lwd=6)
abline(0,1)
#============ Question 3 ============#
Lag1 = as.numeric(GSPC.ret[-c(3971)])^2

#GARCHfit

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
             x = Lag1, y = as.numeric(GSPC.ret[-c(1)]) )

# RNN
rnn.fwdprop = function(x, w){
  n = length(x)
  h = matrix(nrow=n)
  h[1,1] = mean(x)
  for(k in 2:n){
    h[k,1] = actfun(cbind(1,x[k],h[k-1, 1]) %*% w)
  }
  
  y = h
  list(output = y)
}

rnnloss.fun = function(init.w, x, y){
  y.hat = rnn.fwdprop(x,init.w)$output
  return( -sum( log( dnorm(y,0,sqrt(y.hat)) ) ) )
}

garch.rnn = optim(par = init.guess, fn=rnnloss.fun,
              x = Lag1, y = as.numeric(GSPC.ret[-c(1)]) )

# compare estimated parameters
garch$par
garch$par[2] + garch$par[3]

garch.rnn$par
garch.rnn$par[2] + garch.rnn$par[3]

# Plot for a)

beta.val = rep(mean(Lag1),length(Lag1))
for (i in 2:length(Lag1)){
  beta.val[i] = garch.rnn$par[1] + garch.rnn$par[2] * Lag1[i] + garch.rnn$par[3] * beta.val[i-1]
}

y.hatrnn = garch.rnn$par[1] +  
  garch.rnn$par[2]*Lag1 + 
  garch.rnn$par[3]*beta.val

beta.val = rep(mean(Lag1),length(Lag1))
for (i in 2:length(Lag1)){
  beta.val[i] = garch$par[1] + garch$par[2] * Lag1[i] + garch$par[3] * beta.val[i-1]
}

y.hat.g = garch$par[1] +  
  garch$par[2]*Lag1 + 
  garch$par[3]*beta.val

par(mfrow=c(1,2))
plot(y = y.hatrnn, x = y.hat.g, ylab="Recurrent Neural Network", xlab = "GARCH(1,1)",
     col="blue",main="RNN vs GARCH(1,1) for GSPC",lwd=6)
abline(0,1)
