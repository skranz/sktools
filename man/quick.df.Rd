\name{quick.df}
\alias{quick.df}
\title{A function that generates a data.frame more quickly from vectors of equal lengths}
\usage{
quick.df(...)
}
\arguments{
  \item{...}{vectors that have equal lengths and will be
  combined to a data frame}
}
\value{
a data.frame
}
\description{
The function can run substantially quicker than
data.frame(...), since no checks will be performed. Yet,
the function only works if all vectors have the same
length.
}

\examples{\dontrun{
  
  df = quick.df(b=1:5,c=1:5)  
  df
  
  library(rbenchmark)
}}
