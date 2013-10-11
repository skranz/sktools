\name{clone.list}
\alias{clone.list}
\title{#Set and retrieve global options for restore points
#
#}
\usage{
  clone.list(li, use.copied.ref = FALSE)
}
\arguments{
  \item{options}{a list of options that shall be set.
  Possible options are listed below #}

  \item{storing}{TRUE / FALSE enable or disable storing of
  options, setting storing = FALSE basicially turns off
  debugging via restore points #}

  \item{use.browser}{TRUE/FALSE If FALSE (default), when
  options are restored they are simply copied into the
  global environment and the R console is directly used for
  debugging. If TRUE a browser mode will be started when
  objects are restored. It is still possible to parse all R
  commands into the browser and to use copy and paste. To
  quit the browser press ESC in the R console. The
  advantage of the browser is that all objects are stored
  in a newly generated environment that mimics the
  environemnt of the original function, i.e. global
  varariables are not overwritten. Furthermore in the
  browser mode, one can pass the ... object to other
  functions, while this does not work in the global
  environment. The drawback is that at least on my computer
  the approach does not currently work under RStudio, which
  crashes when it is attempted to parse a command from the
  console using the parse() command. #}

  \item{store}{if FALSE don't store objects if
  restore.point or store.objects is called. May save time.
  If TRUE (default) turn on storage again. #}

  \item{name}{key under which the objects are stored. For
  restore points at the beginning of a function, I would
  suggest the name of that function. #}

  \item{deep.copy}{if TRUE (default) try to make deep
  copies of objects that are by default copied by
  reference. Works so far for environments (recursivly) and
  data.tables. The function will search lists whether they
  contain reference objects, but for reasons of speed not
  yet in other containers. E.g. if an evironment is stored
  in a data.frame, only a shallow copy will be made.
  Setting deep.copy = FALSE may be useful if storing takes
  very long and variables that are copied by reference are
  not used or not modified. #}

  \item{force}{store even if set.storing(FALSE) has been
  called #}

  \item{name}{key under which the objects are stored,
  typical the name of the calling function. If name is NULL
  by default the name of the calling function is chosen #}

  \item{deep.copy}{if TRUE (default) variables that are
  copied by reference (in the moment environments and
  data.tables) will be stored as deep copy. May take long
  for large variables but ensures that the value of the
  stored variable do not change #}

  \item{force}{store even if do.store(FALSE) has been
  called #}

  \item{store.if.called.from.global}{if the function is
  called from the global environment and
  store.if.called.from.global FALSE (default) does not
  store objects when called from the global environment but
  does nothing instead. #}
}
\value{
  returns nothing, just called for side effects #
}
\description{
  #Set and retrieve global options for restore points # #
}

