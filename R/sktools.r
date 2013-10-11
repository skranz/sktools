#' Draws a base plot with a legend to the right, must play around with width to get a good looking result
#' @export
plot.with.legend = function(plot.expr,legend, fill, width = 5,bty="n",...) {
  org.par = par(no.readonly = TRUE)
  
  mar = par()$mar
  mar[4] = 2+width
  par(xpd=TRUE, mar = mar) # this is usually the default
  
  eval(plot.expr)
  
  xylim = par("usr")
  x = xylim[2]
  y = xylim[4]
  legend(x=x,y=y,legend=legend,fill=fill,bty=bty,xpd=TRUE,...)
  par(org.par)
}


#' Get the environment in which this function is called
#' @export
currentenv = function() {
  sys.parent(1)
}

to.date.time = function(x, orders = c(c(date.orders),outer(date.orders,time.orders,paste,sep="")),quiet  = FALSE, tz="UTC",locale=Sys.getlocale("LC_TIME"), date.orders=c("dmy","dmY","ymd","Ymd","mdy","mdY"), time.orders =  c("r","R","T","HMSOS")) {
  restore.point("to.date.time")
  require(lubridate)
  if (is.factor(x))
    val = as.character(val)
  ret = tryCatch(as.POSIXct(parse_date_time(x,orders=orders,tz=tz,locale=locale,quiet=quiet)), error = function(e) rep(NA,length(x)))
  ret
}

#' Try to transform a vector or columns that are stored as characters or factors of a data.frame automatically in the approbriate types
#' 
#' So far just attempts to convert date and date.times
#' 
#' @param x a vector or data frame that shall be converted
#' @param max.failure.rate maximum share of rows that can be failed to convert so that conversion still takes place
#' @export
automatic.type.conversion = function(x,max.failure.rate = 0.3,quiet=FALSE,name="",cols=NULL,date.class="POSIX.ct",date.time.class="POSIX.ct",factorsAsStrings=FALSE,stringsAsFactors=FALSE,thousand.sep=NULL,...) {
  restore.point("automatic.type.conversion")
  
  # When called with a data.frame convert seperately
  if (is.data.frame(x)) {
    restore.point("automatic.type.conversion.df")
    
    df = x
    names = colnames(df)
    if (is.null(names)) {
      names = paste("Column", 1:NCOL(df))
    }
    names(names) = colnames(df)
    if (is.null(cols)) {
      cols = 1:NCOL(df)
    }
    for (col in cols) {
      df[,col] <- automatic.type.conversion(df[,col],max.failure.rate = max.failure.rate,quiet=quiet,name=names[col],cols=NULL,date.class=date.class,date.time.class=date.time.class,factorsAsStrings=factorsAsStrings,stringsAsFactors=stringsAsFactors,thousand.sep=thousand.sep,...)
    }
    return(df)
  }
  
  # Convert a vector
  org.class = class(x)[1]
  if (!is.character(x) & !is.factor(x)) {
    if (!quiet) {
      message(name, " remains ", org.class)
    }
    return(x)
  }
  org.x = x
  if (is.factor(x)) {
    org.class = "factor"
    x = as.character(x) 
  } else {
    org.class = "character"
  }
  
  org.na = which(is.na(x))
  
  x.cand = list()
  # Date time
  x.cand[[1]] = to.date.time(x,quiet=quiet,...)
  # Numeric
  
  
  
  if (!is.null(thousand.sep)) {
    x.mod = gsub(thousand.sep,"",x)
  } else {
    x.mod = x
  }  
  x.cand[[2]] = suppressWarnings(tryCatch(as.numeric(x.mod), error = function(e) return(rep(NA,length(x.cand)) )))
  
  
  num.na = sapply(x.cand,function(x) sum(is.na(x)))
  best.cand = which.min(num.na)
  x.convert = x.cand[[best.cand]]  
  new.na = setdiff(which(is.na(x.convert)), org.na)
  
  if (length(new.na) / (length(x)-length(org.na)) < max.failure.rate) {
    x = x.convert
    if (!quiet) {
      message(name," from ", org.class, " to ", class(x)[1])
    }
    if (length(new.na)>0) {
      message("rows not transformed: ", paste(new.na, collapse=","))
    }
  } else {
    if (org.class == "factor" & factorsAsStrings) {
      if (!quiet) {
        message(name," from factor to character")
      }
    } else if (org.class == "character" & stringsAsFactors) {
      if (!quiet) {
        message(name," from character to factor")
      }
      x = as.factor(x)
    } else {
      if (!quiet) {
        message(name," remains ", org.class)
      }
      x = org.x
    }
  }
  x
}

examples.automatic.type.conversion = function() {
  df = data.frame(text=c("c","b","a"),mixed=as.factor(c("a",NA,56)),
    dates = c("1.1.1975","1.1.2010","8.3.1999"),
    date.times = c("1.1.1975 00:10","2010-5-5 12pm","4:30:15"),
    times = c("00:10","12pm",5),
    char.num = as.factor(c("1973","1972","3")),
    half.num = c("145","..","1345")
  )
  df =automatic.type.conversion(df,quiet=FALSE)
  d = df$dates
  d[1]<"1.1.2012"
}

#' Set specified names for x and return the named object
#' @export
named = function(x,names,colnames,rownames) {
  if (!missing(names))
    names(x) = names
  if (!missing(colnames))
    colnames(x) = colnames
  if (!missing(rownames))
    rownames(x) = rownames
  x
}

examples.named = function() {
  named(1:3,c("A","B","C"))
}


#' Convert data in matrix format to grid format with key columns corresponding to for row and col names and a value colum
#' @param dat a data frame or matrix in matrix format
#' @param row.var name of the variable corresponding to different rows
#' @param col.var name of the variable corresponding to different columns
#' @param val.var name of the variable corresponding to the values of the matrix
#' @param row.values values of the row variable
#' @param col.values values of the column variable
#' @export
matrix.to.grid = function(dat, row.var="row", col.var="col", val.var="value", row.values = rownames(dat), col.values=colnames(dat)) {
  
  nr = NROW(dat); nc = NCOL(dat)
  
  if (is.null(row.values) & nr > 0)
    row.values = 1:nr
  if (is.null(col.values) & nc > 0)
    col.values = 1:nc
  row.col = rep(row.values, times=nc)
  col.col = rep(col.values, each=nr)
  if (is(dat,"matrix")) {
    val.col = dat
    dim(val.col) = length(val.col)
  } else {
    val.col = unlist(dat)
  }
  df = data.frame(row.col,col.col,val.col)
  colnames(df) = c(row.var,col.var,val.var)
  df 
}
