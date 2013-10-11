\name{get.coef.and.se}
\alias{get.coef.and.se}
\title{Returns estimated coefficients and standard errors from a regression model. One can also specify linear or non-linear transformations of the original coefficients, standard errors are then computed using the delta method.}
\usage{
  get.coef.and.se(reg, trans.coef = NULL,
    coef. = coef(reg), vcov. = vcov(reg))
}
\arguments{
  \item{reg}{results of any estimation (like lm) that has
  methods coef(reg) and vcov(reg)}

  \item{trans.coef}{a list}
}
\description{
  Returns estimated coefficients and standard errors from a
  regression model. One can also specify linear or
  non-linear transformations of the original coefficients,
  standard errors are then computed using the delta method.
}

\examples{\dontrun{
  get.coef.and.se(m1,trans.coef = list(ratio="t1/t2",prod="t1*t2",inv_t1="1/t1"))
  
  coef(m1)["(Intercept)"]
  se(m1)
  ?coef
  
  deltaMethod(m1, "b1+b2", parameterNames= paste("b", 0:2, sep="")) 
  deltaMethod(m1, "t1/t2") # use names of preds. rather than coefs.
  deltaMethod(m1, "t1/t2", vcov=hccm) # use hccm function to est. vars.
  # to get the SE of 1/intercept, rename coefficients
  deltaMethod(m1, "1/b0", parameterNames= paste("b", 0:2, sep=""))
  # The next example calls the default method by extracting the
  # vector of estimates and covariance matrix explicitly
  deltaMethod(coef(m1), "t1/t2", vcov.=vcov(m1))
}}
