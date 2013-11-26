\name{ampl.run.local}
\alias{ampl.run.local}
\title{Solves an AMPL model using a local installation of AMPL}
\usage{
  ampl.run.local(name = "", path = getwd(),
    run.file = paste(path, "/", name, ".run", sep = ""),
    display = TRUE)
}
\description{
  You need to have a local AMPL installation with the
  corresponding solvers. It is assumed that a call to ampl
  finds the executable file, i.e. in Windows you have to
  add the AMPL directory to the system PATH variable
}

\examples{\dontrun{
  # Model of power plant investments and dispatch included in package
  mod.file = paste(.path.package(package = "rampl"),"/data/cournot.mod",sep="")
  dat.file = paste(.path.package(package = "rampl"),"/data/cournot.dat",sep="")
  run.file = paste(getwd(),"/cournot.run",sep="")
  
  ampl.make.run.file(name="cournot", options=c("option solver minos;"), mod.file=mod.file,dat.file=dat.file,run.file=run.file)
  ret = ampl.run.local(name="cournot", display=TRUE, run.file=run.file)
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
