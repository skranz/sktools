\name{gmpl.solve}
\alias{gmpl.solve}
\title{Solve a GMPL problem using glpkAPI}
\usage{
  gmpl.solve(mod.file = NULL, dat.file = NULL, lp = NULL,
    delete.lp = is.null(lp), adapt.sol = TRUE)
}
\arguments{
  \item{mod.file}{path of the .mod file in which the gmpl
  model is specified}

  \item{dat.file}{path of the .dat file in which the gmpl
  data is specified}

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

