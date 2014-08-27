\name{run.scenarios}
\alias{run.scenarios}
\title{A helper function to simulate different scenarios. So far the function is just a simple wrapper to mapply.}
\usage{
run.scenarios(fun, par, show.progress.bar = interactive(), other.par = NULL,
  ...)
}
\arguments{
  \item{fun}{a function that runs the simulation for given
  set of parameters and returns the results as a
  data.frame. The data.frame must have the same number of
  columns, for all possible parameter combinations, the
  number of rows can differ, however.}

  \item{par}{a list of the scalar parameters used by
  sim.fun. If some parameters in the list are vectors, run
  the simulation for every combination of parameter
  values.}

  \item{show.progress.bar}{= TRUE or FALSE. Shall a
  progress bar be shown?}

  \item{other.par}{a list of other parameters that will be
  used by sim.fun that do not vary across simulations and
  won't be specified in the results.}
}
\value{
returns a data.frame that combines the results for all
scenarios replication of each dgp and each estimation
procedure and each parameter constellation. The first
column is an integer number specifying the number of the
scenario. Then columns follow for each paramter. The
remaining columns are the returned values by sim.fun. The
results can be conviniently analysed graphically, e.g. with
ggplot2. Aggregate statistics can for example be computed
by quick.by.
}
\description{
A helper function to simulate different scenarios. So far
the function is just a simple wrapper to mapply.
}

\examples{\dontrun{
  # Compute some values of a linear and quadratic function
  f = function(a,b,c) {
    x = seq(0,1,length=5)
    return(data.frame(x=x,lin=a+b*x,quad=a+b*x+c*x^2))
  }
  ret = run.scenarios(f,par=list(a=c(0,2),b=c(-1,1,2),c=c(0,1,3,4)))
  ret
  
  # Plot using ggplot2
  library(ggplot2)
  qplot(x=x,y=quad,data=ret, facets= a ~ b, color=as.factor(c),geom="line")
  
}}
