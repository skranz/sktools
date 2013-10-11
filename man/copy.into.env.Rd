\name{copy.into.env}
\alias{copy.into.env}
\title{#Restore stored objects by copying them into the global environment
#
#}
\usage{
  copy.into.env(source = sys.frame(sys.parent(1)),
    dest = sys.frame(sys.parent(1)), names = NULL,
    exclude = NULL)
}
\arguments{
  \item{name}{name under which the variables have been
  stored #}

  \item{dest}{environment into which the stored variables
  shall be copied. By default the global environment. #}

  \item{was.forced}{flag whether storage of objects was
  forced. If FALSE (default) a warning is shown if
  restore.objects is called and is.storing()==FALSE, since
  probably no objects have been stored. #}

  \item{source}{a list or environment from which objects
  are copied}

  \item{the}{enviroenment into which objects shall be
  copied}

  \item{names}{optionally a vector of names that shall be
  copied. If null all objects are copied}

  \item{exclude}{optionally a vector of names that shall
  not be copied}
}
\value{
  returns nothing but automatically copies the stored
  variables into the global environment #
}
\description{
  #Restore stored objects by copying them into the global
  environment # #
}

