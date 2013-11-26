\name{ampl.run.neos}
\alias{ampl.run.neos}
\title{Solves an AMPL model remotely using the NEOS Server}
\usage{
  ampl.run.neos(name = "", category = "cp",
    solver = "PATH", path = getwd(), wait = TRUE,
    mod.file = paste(path, "/", name, ".mod", sep = ""),
    dat.file = paste(path, "/", name, ".dat", sep = ""),
    run.file = paste(path, "/", name, ".run", sep = ""),
    log.file = paste(path, "/log_", name, "_", solver, ".txt", sep = ""))
}
\arguments{
  \item{name}{the model name}

  \item{category}{category of the optimization problem,
  call neos.ampl.solvers() for an overview}

  \item{solver}{desired solver, call neos.ampl.solvers()
  for a list}

  \item{path}{path in which mod.file, dat.file and run.file
  can be found}

  \item{wait}{default=TRUE shall R wait until NEOS returns
  the solution (may take some time)}
}
\description{
  Solves an AMPL model remotely using the NEOS Server
}

\examples{\dontrun{
  # Model of power plant investments and dispatch included in package
  mod.file = paste(.path.package(package = "rampl"),"/data/cournot.mod",sep="")
  dat.file = paste(.path.package(package = "rampl"),"/data/cournot.dat",sep="")
  run.file = paste(getwd(),"/cournot.run",sep="")
  ampl.make.run.file(name="cournot", neos=TRUE, mod.file=mod.file,dat.file=dat.file,run.file=run.file, options=c(""))
  ret = ampl.run.neos(name="cournot", category="nco", solver="MINOS", mod.file=mod.file, dat.file=dat.file, run.file=run.file)
  ret
  
  # Solve for different parameter values
  n = 2
  sets = list(N=1:n)
  param = list(a=1,b=1,c=c(0.1,0.1))
  
  dat.file = paste(getwd(),"/cournot.dat",sep="")
  run.file = paste(getwd(),"/cournot.run",sep="")
  ampl.make.run.file(name="cournot", options=c("option solver minos;"), mod.file=mod.file,dat.file=dat.file,run.file=run.file)
  
  solve.cournot = function(c1=0,c2=0) {
    param$c = c(c1,c2)
    ampl.make.dat.file(mod.file=mod.file,dat.file=dat.file,sets = sets, param=param)
    ret = ampl.run.local(name="cournot", display=FALSE, run.file=run.file)
    t(ret$q)
  }
  solve.cournot(c1=0.1,c2=0)
  library(sktools)
  ret = run.scenarios(solve.cournot, par=list(c1=seq(0,1,length=10),c2=0))
  colnames(ret)=c("scen.id","c1","c2","q1","q2")
  ret
  
}}
