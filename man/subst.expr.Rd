\name{subst.expr}
\alias{subst.expr}
\title{Substitute some pattern by a list of alternatives}
\usage{
subst.expr(expr.str, subst, collapse = ",", add.list = FALSE)
}
\description{
Substitute some pattern by a list of alternatives
}

\examples{\dontrun{
  subst.expr("mean.ATTR=mean(ATTR)",subst=list(ATTR=c("hp","li")))
}}
