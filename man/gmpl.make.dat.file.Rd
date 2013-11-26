\name{gmpl.make.dat.file}
\alias{gmpl.make.dat.file}
\title{Generates a GMPL data file}
\usage{
  gmpl.make.dat.file(sets = NULL, param = NULL, mod.file,
    dat.file)
}
\arguments{
  \item{sets}{a list with the sets used by the gmpl model}

  \item{param}{a list with the parameters used by the gmpl
  model}

  \item{mod.file}{path of the .mod file in which the gmpl
  model is specified}

  \item{dat.file}{path of the .dat file in which the data
  shall be written}
}
\description{
  Generates a GMPL data file for the model specified in
  dat.file sets and param are lists that contain the values
  of the sets and parameters that are specified in the GMPL
  model
}

\examples{\dontrun{
  
  # Model of power plant investments and dispatch included in package
  mod.file = paste(.path.package(package = "rgmpl"),"/data/power.mod",sep="")
    
  # Name of dat file, will be generated locally
  dat.file = "power.dat"
  
  # Example data
  
  # Sets
  PLANTS = c("coal","gas")
  PERIODS = 1:4
  sets = list(PLANTS=PLANTS,PERIODS=PERIODS)
  
  # Parameters
  fc = c(12,6)  # fixed cost 
  vc = c(18,30) # variable cost
  load = c(30,50,25,20) # electricity demand
  T = length(PERIODS)
  param = list(vc=vc,fc=fc,load=load,T=T)
  
  # Generate a GMPL .dat file
  gmpl.make.dat.file(sets=sets,param=param,mod.file=mod.file, dat.file=dat.file)
  
  # Solve the model
  res = gmpl.solve(mod.file=mod.file,dat.file=dat.file, delete.lp =FALSE)
  res
  
  # Show production levels graphically
  library(ggplot2)
  qplot(data=res$sol$q, x=PERIODS,y=q,fill=PLANTS,geom="bar",stats="identity", xlab="Period",ylab="Production")
  
}}
