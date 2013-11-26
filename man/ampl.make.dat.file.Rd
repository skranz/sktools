\name{ampl.make.dat.file}
\alias{ampl.make.dat.file}
\title{Generates an AMPL data file}
\usage{
  ampl.make.dat.file(...)
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
  Generates a AMPL data file for the model specified in
  dat.file sets and param are lists that contain the values
  of the sets and parameters that are specified in the GMPL
  model
}

