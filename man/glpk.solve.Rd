\name{glpk.solve}
\alias{glpk.solve}
\title{Solve a GLPK linear problem}
\usage{
  glpk.solve(lp = NULL, delete.lp = TRUE)
}
\arguments{
  \item{lp}{a GLPK problem generated e.g. by a call to
  gmpl.load.problem}

  \item{delete.lp}{default = TRUE shall the problem lp be
  removed from memory after it has been solved?}
}
\description{
  Solve a GLPK linear problem
}

