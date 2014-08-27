\name{robust.vcov}
\alias{robust.vcov}
\title{Computes robust variance covariance estimator of a model
works at least with lm (OLS)
Goal: Make it work correctly for ivreg (instrumental variables)}
\usage{
robust.vcov(fm, robust = c("HAC", "HC", "cluster", "not"), cluster1 = NULL,
  cluster2 = NULL, ...)

robust.vcov(fm, robust = c("HAC", "HC", "cluster", "not"), cluster1 = NULL,
  cluster2 = NULL, ...)
}
\description{
Adapted from code by Drew Dimmery
http://www.drewdimmery.com/robust-ses-in-r/

Computes robust standard errors
}

