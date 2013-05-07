\name{simulation.study}
\alias{simulation.study}
\title{A helper function to conduct a simulation study for different parameter combinations}
\usage{
  simulation.study(fun, par = NULL, repl = 1, ...,
    show.progress.bar = interactive(), add.run.id = TRUE,
    colnames = NULL, same.seeds.each.par = TRUE,
    seeds = floor(runif(repl, 0, .Machine$integer.max)),
    LAPPLY = lapply)
}
\arguments{
  \item{fun}{a function that returns a vector, data.frame
  or matrix}

  \item{par}{an optional list that specifies parameters for
  different scenarios. fun will be called repl times for
  each parameter combinantion of the parameters specified
  in par}

  \item{repl}{for Monte-Carlo simulation the number of
  times fun is called for each parameter combinantion}

  \item{...}{additional parameters that will be used by
  fun}

  \item{show.progress.bar}{shall a progress bar been
  shown?}

  \item{add.run.id}{shall a column be added that is a
  unique value for every unique call of fun}

  \item{colnames}{optionally a vector of colnames for the
  results returned by fun. It is quicker to set colnames
  just in the end, instead of fun setting names.}

  \item{same.seeds.each.par}{if TRUE (default) then we set
  for all parameter combinations with which the function is
  called the same random seed in the i'th replication. One
  effect e.g. is that if we draw some disturbances eps in
  our simulation the same disturbances will be drawn in the
  i'th repitition for all parameter values, we study. This
  typically facilitates the analysis of comparative
  statics.}

  \item{seeds}{if same.seeds.each.par = TRUE one can
  manually provide a vector of random seeds of length
  repl.}

  \item{LAPPLY}{a function that has the same behavior as
  lapply. One can use an different function, e.g. in order
  to parallelize the execution when running a simulation on
  a computer cluster.}
}
\value{
  returns a data.frame that combines the results of all
  calls to fun and adds the corresponding parameter
  combinantion and an index for the actual replication. The
  data.frame can be conviniently analysed graphically, e.g.
  with ggplot2
}
\description{
  A helper function to conduct a simulation study for
  different parameter combinations
}

\examples{
  
  # A function that simulates demand
  demand.sim = function(beta0=100,beta1=-1,sigma.eps=0.4,T=200, p.min=0, p.max= (-beta0/beta1)) {  
    # Draw prices always in the same fashion, with a fixed random seed
    p=with.random.seed(runif(T,p.min,p.max), seed=123456789)
    eps = rnorm(T,0,sigma.eps) # Demand Shock for each market
    # Realized demand
    q = beta0 + beta1* p+eps # = beta0 + beta1*p + eps
    #data.frame(p=p,q=q,eps=eps)    
    quick.df(p=p,q=q,eps=eps)       
  }
  # A function that simulates data and performs OLS estimation on the simulated data
  est.sim.fun = function(beta0=100,beta1=-1,...) {
    df = demand.sim(...)
    y = df$q
    X = cbind(1,df$p)
    reg = lm.fit(x=X,y=y)
    quick.df(name=c("beta0.hat","beta1.hat"),coef = coef(reg),true.coef = c(beta0,beta1),se = sqrt(diag(vcov.lm.fit(reg))))
  }
  demand.sim(T=3)
  set.seed(1234)
  est.sim.fun(T=20)

  simulation.study(fun=demand.sim, par=list(T=c(1,2)), repl=2, add.run.id=TRUE)
  
  
  sim = simulation.study(fun=est.sim.fun, par=list(T=c(10,50,100,200), sigma.eps=c(1,3)), repl=50, add.run.id=TRUE, same.seeds.each.par=TRUE)
  
  head(sim)
  
  
  library(ggplot2)  
  
  # Select only the estimate of beta1.hat
  dat = sim[sim$name=="beta0.hat",]
  
  qplot(coef,geom="density",alpha = I(0.6),data=dat, group=T, fill=as.factor(T), facets=sigma.eps~.)  #+ coord_cartesian(ylim = c(0, 75))

  # Compute sample MSE and Bias
  agg = quick.by(dat,list(
        mse = mean((coef-true.coef)^2),
        bias=mean(coef-true.coef)),
        by=c("T","sigma.eps"))
  agg
  
  qplot(y=est.mse,x=T,data=agg,geom="line",group=sigma.eps, color=as.factor(sigma.eps),ylab="Estimated MSE",size=I(1.2))
  qplot(y=est.bias,x=T,data=agg,geom="line",group=sigma.eps, color=as.factor(sigma.eps), ylab="Estimated bias",size=I(1.2))
  
  ret 
}
