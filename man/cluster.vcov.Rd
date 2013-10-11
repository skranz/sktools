\name{cluster.vcov}
\alias{cluster.vcov}
\title{R function for computing two-way cluster-robust standard errors.}
\usage{
  cluster.vcov(data, fm, cluster1, cluster2 = NULL)
}
\description{
  The code below was adapted by Ian Gow on May 16, 2011
  using code supplied via Mitchell Petersen's website by
  Mahmood Arai, Jan 21, 2008.
}
\details{
  Apart from a little cleanup of the code, the main
  difference between this and the earlier code is in the
  handling of missing values. Look at the file
  cluster.test.R to see example usage. Note that care
  should be taken to do subsetting outside of the call to
  lm or glm, as it is difficult to recon- struct subsetting
  of this kind from the fitted model. However, the code
  does handle transformations of variables in the model
  (e.g., logs). Please report any bugs, suggestions, or
  errors to iandgow
}

