\name{vcov.lm.fit}
\alias{vcov.lm.fit}
\title{Returns the variance-covariance matrix from the return values of a call to lm.fit}
\usage{
\method{vcov}{lm.fit}(lm.fit, qr = lm.fit$qr, residuals = lm.fit$residuals)
}
\arguments{
  \item{lm.fit}{The return value of a call to lm.fit}
}
\value{
the variance covariance of the ols estimate
}
\description{
Is useful in so far that calling lm.fit is much quicker
than calling lm, but the normal vcov function does not work
when calling with the result of lm.fit
}

