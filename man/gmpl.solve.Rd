\name{gmpl.solve}
\alias{gmpl.solve}
\title{Solve a GMPL problem using glpkAPI}
\usage{
  gmpl.solve(mod.file = NULL, dat.file = NULL, sets = NULL,
    param = NULL, lp = NULL, delete.lp = is.null(lp),
    adapt.sol = TRUE)
}
\arguments{
  \item{mod.file}{path of the .mod file in which the gmpl
  model is specified}

  \item{dat.file}{path of the .dat file in which the gmpl
  data is specified. If NULL generate a new .dat file from
  the given sets and param with the same name as the model
  file}

  \item{sets}{a list with the sets used by the gmpl model.
  Needed if no dat.file specified}

  \item{param}{a list with the parameters used by the gmpl
  model. Needed if no dat.file specified}

  \item{lp}{optional a link to the GLPK problem generated
  by gmpl.load.problem}

  \item{delete.lp}{default = TRUE if lp is given, shall it
  be removed from memory after it has been solved?}

  \item{adapt.sol}{default = TRUE shall the solution be
  returned in a more convenient form}
}
\description{
  Solve a GMPL problem using glpkAPI
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
