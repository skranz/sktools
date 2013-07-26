\name{robust.vcov}
\alias{robust.vcov}
\title{Computes robust variance covariance estimator of a model
works at least with lm (OLS)
Goal: Make it work correctly for ivreg (instrumental variables)}
\usage{
  robust.vcov(model,
    type = ifelse(is.null(cluster), "HC1", "cluster"),
    cluster = NULL, ...)
}
\description{
  Adapted from code by Drew Dimmery
  http://www.drewdimmery.com/robust-ses-in-r/
}

