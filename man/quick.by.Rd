% Generated by roxygen2 (4.0.1): do not edit by hand
\name{quick.by}
\alias{quick.by}
\title{Quick version of by using internally data.table}
\usage{
quick.by(df, by = NULL, expr, add.col = FALSE, as.data.table = FALSE,
  keep.row.order = add.col, ...)
}
\arguments{
\item{df}{a data.frame that shall be aggregated / transformed}

\item{by}{either a character vector with the columns of df over which grouping shall take place or a list vectors of size NROW(df) containing group indices}

\item{expr}{a string containing an R expr operating on columns in df that shall be evaluated, corresponds to j parameter in data.table, can be a named list to generate multiple columns (see data.table introduction).}

\item{add.col}{if TRUE the data generated by group-wise evaluation of expr will be cbinded to the corresponding rows in df}
}
\value{
a data.frame with values of indices in by and returning expressions
}
\description{
Quick version of by using internally data.table
}
\author{
Sebastian Kranz
}

\examples{\dontrun{
  # Simulate a data set of time needed to run a given distance
  # individuals differ by age and gender
  T <- 10
  id <- 1:T
  age    <- sample(10:12, T, replace=TRUE)
  gender <- sample(c("M","F"), T, replace=TRUE)
  time <- runif(T,10,100) - sqrt(age) - (gender=="M")*1
  df <- data.frame(id,age,gender,time)
  
  # Mean time for each gender group
  quick.by(df,"avgtime=mean(time)",by=c("gender"))
  # Mean time for each age / gender group
  quick.by(df,"avgtime=mean(time)",by=c("age","gender"))
  
  # Mean time for each age / gender group and number of ind
  quick.by(df,"avgtime=mean(time), groupsize=length(time)",by=c("age","gender"))
  
  
  # Best time in each age / gender group and identity of winner
  quick.by(df,"best.time=min(time),id.winner=id[which.min(time)]",by=c("age","gender"))
  
  # Add best.time to original df
  quick.by(df,"best.time=min(time), group.size=length(time)",by=c("age","gender"), add.col=TRUE)
  
  # Add best time and groupsize to original df
  quick.by(df,"best.time:=min(time)",by=c("age","gender"))
  
  
  # Add best time within group and groupsize to original df and compute gap to best for each individual
  new.df <- quick.by(df,"best.time=min(time), group.size=length(time)",by=c("age","gender"), add.col=TRUE)
  
  # Compute gap to best.time
  new.df$gap <- new.df$time - new.df$best.time
  new.df
  
  
  # Compare speed of quick.by with other data aggregation tools
  library(plyr)
  
  T <- 1000
  ind <- sample(1:1000, T, replace=TRUE)
  val <- 1:T*0.01
  df <- data.frame(ind,val)
  dt <- as.data.table(df)
  library(rbenchmark)
  benchmark(
    data.table = dt[,list(sum=sum(val)),by="ind"],
    data.table.add = dt[,sum:=sum(val),by="ind"],
    quick.by=quick.by(df,by="ind","sum=sum(val)"),
    quick.by.add=quick.by(df,by="ind","sum=sum(val)",add.col=TRUE),
    ddply = ddply(df,"ind",function(df) sum(df$val)),
    by = by(df,df$ind,function(df) sum(df$val)),
    replications=2,order="relative")
  # While using data.table directly is the fastest method, quick.by is not much slower and substantially faster than other methods. There is still substantial room for speeed improvement for add.col = TRUE
}}
