#' A Wu-Hausman Test for a single endogenous variable
#' 
#' See e.g. Green (2003) Econometric Analysis for a description of the Wu-Hausman-Test
#' 
#' @param y the vector of dependent variables
#' @param X the matrix of explanatory variables (endogenos & exogenous)
#' @param Z the matrix of instruments (excluded & included instruments)
#' @param endo vector or matrix of variables that might be endogenous
wu.hausman.test = function(y,X.exo,X.endo,Z) {
  #restore.point("wu.hausman.test")
  # 1st Stage Regression of 2SLS compute fitted endogenous variables
  reg0 = lm.fit(x=Z,y=X.endo)
  X.endo.hat = reg0$fitted.values

  # Run the regression
  # y = endo.hat * gamma + X*beta  + eps
  # and perform an F-test on whether the coefficients gamma on endo.hat are 
  # significant.
  # The F-test follows closely the description on Wikipedia
  # http://en.wikipedia.org/wiki/F-test#Regression_problems  

  X = cbind(X.exo,X.endo)
  reg1 = lm.fit(x=X,y=y)  # The restricted model
  reg2 = lm.fit(x=cbind(X,X.endo.hat),y=y) # The unrestricted model
  
  RSS1 = sum(reg1$residuals^2)
  RSS2 = sum(reg2$residuals^2)
  
  T = NROW(y)
  K1 = NCOL(X)
  K2 = K1 + NCOL(X.endo.hat)
  
  # Test statistic
  F = ((RSS1-RSS2) / (K2-K1)) / (RSS2/(T-K2))
  # P-values from the F distribution
  p = pf(F,K2-K1,T-K2, lower.tail = FALSE)
  
  # Return a list with test statistic F and the p-value
  res = list(F=F,p.value=p)
  
  # Set a class for the return value
  # in order to print the resulting list
  # using the manual function print.WuHausmanTest below
  class(res)=c("WuHausmanTest")
  res  
}

# This function will be called when the result of
# wu.hausman.test will be printed
print.WuHausmanTest = function(test) {
  str = paste0("Wu-Hausman-Test for Endogeniety",
"\n Null Hypothesis: All variables are exogenous.",
"\n Test Statistic: F = ",test$F,
"\n           p-value = ",round(test$p.value,10))
  message(str)
}

# Some examples of applying the Wu-Hausman Test
# Just run manually the code inside the function
examples.wu.hausman.test = function() {
  library(sktools)  
  # Ice cream model
  T = 100
  beta0 = 100; beta1  = -1; beta2 = 40
  
  s   = rep(0:1, length.out=T) # Season: winter summer fluctuating
  eps = rnorm(T,0,2)
  c   = runif(T,10,20)
  
  # Optimal price if the firm knows season s and u
  p = (beta0+beta2*s+eps - beta1*c) / (2*-beta1)
  
  # Alternatively: prices are a random markup above cost c
  p = c*runif(T,1,1.1)
  
  # Compute demand
  q = beta0 + beta1*p+ beta2*s + eps
  
  # Matrix of instruments
  Z = cbind(1,c,s)
  
  # Run the Wu-Hausman test for testing endogeniety of p
  wu.hausman.test(y=q,X.exo=cbind(1,s),X.endo=p,Z=Z)

  ###########################################################################################
  # Run a systematic simulation study of 
  # the Wu-Hausman Test
  ###########################################################################################

  # This function simulates and estimates a demand function and
  # returns the p-value of a Wu-Hausman test for endogeniety of the price
  sim.and.test = function() {
    # Ice cream model
    T = 100
    beta0 = 100; beta1  = -1; beta2 = 40
    
    s   = rep(0:1, length.out=T) # Season: winter summer fluctuating
    eps = rnorm(T,0,2)
    c   = runif(T,10,20)
    
    # Optimal price if the firm knows season s and u
    p = (beta0+beta2*s+eps - beta1*c) / (2*-beta1)
    
    # Alternatively: prices are a random markup above cost c
    p = c*runif(T,1,1.1)
    
    # Compute demand
    q = beta0 + beta1*p+ beta2*s + eps
    
    # Matrix of instruments
    Z = cbind(1,c,s)
    
    # Run the Wu-Hausman test for testing endogeniety of p
    wu.hausman.test(y=q,X.exo=cbind(1,s),X.endo=p,Z=Z)$p.value
  }

  # Perform a simulation study of our implementation of the Wu-Hausman test
  sim = simulation.study(sim.and.test,repl=1000)

  # I just need to adapt the results (won't be neccessary in a
  # newer version of sktools)
  sim = as.data.frame(sim)
  colnames(sim)[2] = "p.value"
  
  # Show the results and draw a histogram
  head(sim)
  hist(sim$p.value)
  # If the data is generated such that the Null hypothesis that
  # p is exogenous holds, then the p-values should be uniformly
  # distributed.
  
  # Use a Kolmogorov-Smirnov Test for whether p-values are uniformely distributed 
  #ks.test(sim$p.value,"punif",min=0,max=1)
  
  
}
