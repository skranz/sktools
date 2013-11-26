\name{ampl.make.run.file}
\alias{ampl.make.run.file}
\title{Generate a default run file for a given AMPL model}
\usage{
  ampl.make.run.file(name, run.name = name, options = "",
    var.out = NULL, neos = FALSE, path = getwd(),
    mod.file = paste(path, "/", name, ".mod", sep = ""),
    run.file = paste(path, "/", run.name, ".run", sep = ""),
    dat.file = paste(path, "/", run.name, ".dat", sep = ""))
}
\arguments{
  \item{neos}{if true a run file for the neos server is
  created, otherwise for a local call to AMPL}

  \item{path}{path in which mod.file, dat.file and run.file
  can be found}
}
\description{
  Generate a default run file for a given AMPL model
}

