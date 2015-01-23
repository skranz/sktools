
#' Transform data set for analysis in growth regressions
#'
#' Generate growth rates, absolute gains, average values, first and last values in time windows of size ahead+1
#' @param dat a data frame increasingly sorted in time with no gaps
#' @param ahead for how many periods ahead shall growth rates etc be computed
#' @param trans.vars variables that shall be transformed . By default all numeric variables in dat
#' @param fixed.vars variables whose first value in each time window that shall be added to the results without any transformation.
#' @param overlapping (default=TRUE) shall time windows be overlapping or not
#' @param slide the distance of window starts is by default 1 if overlapping == TRUE and ahead if overlapping==FALSE
transform.for.growth.analysis <- function (dat, ahead = 1, trans.vars = names(which(sapply(dat, 
    is.numeric))), fixed.vars = setdiff(colnames(dat), trans.vars), slide=ifelse(overlapping,1,ahead),overlapping = TRUE) 
{
  restore.point("transform.for.growth")

  T = NROW(dat)
  if (T < 1 + ahead) 
      return(NULL)
  srows = seq.int(from = 1, to = T - ahead, by = slide)
  erows = srows + ahead
  n = length(srows)
  #var = "row"
  #x = dat$row
  transform.x = function(x, var = "var") {
      start = x[srows]
      end = x[erows]
      gain = (end - start)/ahead
      growth = (1 + (end - start)/start)^(1/ahead) - 1
      
      # compute mean for all windows
      cs = c(0,cumsum(x))
      avg = (cs[erows+1] - cs[srows])/(ahead + 1)
      
      # create data frame
      res = quick.df(start = start, end = end, avg = avg, 
          gain = gain, growth = growth)
      colnames(res) = paste0(var, "_", colnames(res))
    
      res
  }
  li = lapply(trans.vars, function(var) {
      transform.x(dat[[var]], var)
  })
  dw = do.call(cbind, li)
  res = cbind(dat[srows, fixed.vars, drop = FALSE], dw)
  res
}