examples.add_lags = function() {
  T = 5; N = 4
  df = data_frame(i = rep(1:N, times=T),t=rep(1:T,each=N),x=runif(T*N),y=runif(T*N))
  add_lags(df,var="x",lags=1:3, leads=1:2, groupvar="i",timevar="t")


  add_lags(df,var=c("x","y"),lags=1:3,leads=1, groupvar="i", timevar="t")
}

add_lags = function(data,var,lags=1,leads=NULL, groupvar=NULL, timevar=NULL, is_sorted=is.null(timevar)) {
  library(dplyrExtras)
  if (!is.null(groupvar))
    data = s_group_by(data, groupvar)
  if (!is_sorted)
    data = s_arrange(data, timevar)


  code = NULL
  if (length(lags)>0) {
    gr = expand.grid(var,lags)
    code = c(code,paste0(gr[,1],"_lag",gr[,2]," = lag(",gr[,1],",",gr[,2],")", collapse=", "))
  }
  if (length(leads)>0) {
    gr = expand.grid(var,leads)
    code = c(code,paste0(gr[,1],"_lead",gr[,2]," = lead(",gr[,1],",",gr[,2],")", collapse=", "))
  }

  ungroup(s_mutate(data,code))

}
