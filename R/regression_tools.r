#' An extended version of ivreg from AER that allows robust standard errors
ivregress = function(..., robust="not",cluster1=NULL,cluster2=NULL) {
  fm = ivreg(...)
  if (robust=="not")
    return(fm)
  fm$vcov = robust.vcov(fm,robust=robust,cluster1,cluster2)
  fm
}

#' Computes robust standard errors
#' 
#' 
robust.vcov = function(fm, robust=c("HAC","HC","cluster","not"), cluster1=NULL,cluster2=NULL,...) {
  robust = robust[1]
  
  
  if (robust == "HAC") {
    return(vcovHAC(fm,...))
  } else if (robust=="HC") {
    return(vcovHC(fm,...))
  } else if (robust=="cluster") {
    return(cluster.vcov(fm, cluster1=cluster1,cluster2=cluster2))
  }
}

#' Computes robust standard errors for the coefficients of a fitted model
#' 
robust.se = function(fm, robust=c("HAC","HC","cluster"),...) {
  vcov = robust.vcov(fm,robust,...)
  sqrt(diag(vcov))
}

examples.robust.se = function() {
  
}

#' R function for computing two-way cluster-robust standard errors.
#'
#' The code below was adapted by Ian Gow on May 16, 2011 using code supplied
#' via Mitchell Petersen's website by Mahmood Arai, Jan 21, 2008. 
#'
#' Apart from a little cleanup of the code, the main difference between this
#' and the earlier code is in the handling of missing values. Look at the file
#' cluster.test.R to see example usage. Note that care should be taken to 
#' do subsetting outside of the call to lm or glm, as it is difficult to recon-
#' struct subsetting of this kind from the fitted model. However, the code
#' does handle transformations of variables in the model (e.g., logs). Please
#' report any bugs, suggestions, or errors to iandgow@gmail.com. The output has 
#' been tested fairly extensively against output of Mitchell Petersen's 
#' cluster2.ado commmand (hence implicitly against the Matlab and SAS code posted 
#' elsewhere here), but I have not tested the code against code for non-linear 
#' models, such as logit2.ado.

#' See: Thompson (2006), Cameron, Gelbach and Miller (2006) and Petersen (2010).
#' and Gow, Ormazabal, and Taylor (2010) for more discussion of this code
#' and two-way cluster-robust standard errors.

#' The arguments of the function are data, fitted model, cluster1 and cluster2
#' You need to install packages `sandwich' by Thomas Lumley and Achim Zeileis and
#' `lmtest' by Torsten Hothorn, Achim Zeileis, Giovanni Millo and David Mitchell.
#' (For example, robust install.packages("sandwich") on the R console.)
#' 
#' http://www.people.hbs.edu/igow/GOT/Code/cluster2.R.html
cluster.vcov <- function(data, fm, cluster1, cluster2=NULL) {  
  require(sandwich)
  require(lmtest)
  
  # Calculation shared by covariance estimates
  est.fun <- estfun(fm)
  # est.fun <- sweep(fm$model,MARGIN=2,fm$residuals,`*`)   
  
  # Need to identify observations used in the regression (i.e.,
  # non-missing) values, as the cluster vectors come from the full 
  # data set and may not be in the regression model.
  # I use complete.cases following a suggestion from 
  # Francois Cocquemas <francois.cocquemas@gmail.com>
  inc.obs <- complete.cases(data[,names(fm$model)])
  # inc.obs <- !is.na(est.fun[,1])
  # est.fun <- est.fun[inc.obs,]
  
  # Shared data for degrees-of-freedom corrections
  N  <- dim(fm$model)[1]
  NROW <- NROW(est.fun)
  K  <- fm$rank
  
  # Calculate the sandwich covariance estimate
  cov <- function(cluster) {
    cluster <- factor(cluster)
    
    # Calculate the "meat" of the sandwich estimators
    u <- apply(est.fun, 2, function(x) tapply(x, cluster, sum))
    meat <- crossprod(u)/N
    
    # Calculations for degrees-of-freedom corrections, followed 
    # by calculation of the variance-covariance estimate.
    # NOTE: NROW/N is a kluge to address the fact that sandwich uses the
    # wrong number of rows (includes rows omitted from the regression).
    M <- length(levels(cluster))
    dfc <- M/(M-1) * (N-1)/(N-K)
    dfc * NROW/N * sandwich(fm, meat=meat)
  }
  
  # Calculate the covariance matrix estimate for the first cluster.
  cluster1 <- data[inc.obs,cluster1]
  cov1  <- cov(cluster1)
  
  if(is.null(cluster2)) {
    return(cov1)
  } else {
    # Otherwise do the calculations for the second cluster
    # and the "intersection" cluster.
    cluster2 <- data[inc.obs,cluster2]
    cluster12 <- paste(cluster1,cluster2, sep="")
    
    # Calculate the covariance matrices for cluster2, the "intersection"
    # cluster, then then put all the pieces together.
    cov2   <- cov(cluster2)
    cov12  <- cov(cluster12)
    covMCL <- (cov1 + cov2 - cov12)
    
    # Return the output of coeftest using two-way cluster-robust
    # standard errors.
    return(covMCL)
  }
}
