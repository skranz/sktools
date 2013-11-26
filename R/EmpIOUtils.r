# Author: Sebastian Kranz 
# Some tools used in my courses

#' A helper function to simulate different scenarios. So far the function is just a simple wrapper to mapply.
#' @param fun a function that runs the simulation for given set of parameters and returns the results as a data.frame. The data.frame must have the same number of columns, for all possible parameter combinations, the number of rows can differ, however.
#' @param par a list of the scalar parameters used by sim.fun. If some parameters in the list are vectors, run the simulation for every combination of parameter values.
#' @param show.progress.bar = TRUE or FALSE. Shall a progress bar be shown?
#' @param other.par a list of other parameters that will be used by sim.fun that do not vary across simulations and won't be specified in the results.
#' @return returns a data.frame that combines the results for all scenarios replication of each dgp and each estimation procedure and each parameter constellation. The first column is an integer number specifying the number of the scenario. Then columns follow for each paramter. The remaining columns are the returned values by sim.fun. The results can be conviniently analysed graphically, e.g. with ggplot2. Aggregate statistics can for example be computed by quick.by.
#' @export
run.scenarios = function(fun,par,show.progress.bar =interactive(),other.par=NULL,...) {
  other.par = c(other.par,list(...))
  
  restore.point("run.scenarios")

  par.grid = as.data.frame(expand.grid(par))
  
  if (show.progress.bar) {
    env <- environment()
    pb_Total <- NROW(par.grid)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = pb_Total,
                         style = 3)
    fun.wrapper <- function(...)
    {
      curVal <- get("counter", envir = env)
      assign("counter", curVal +1 ,envir= env)
      setTxtProgressBar(get("pb", envir= env),
                        curVal +1)
      
      ret = fun(...)
      if (!is.data.frame(ret) & !is.matrix(ret))
        error("run.scenarions: fun(...) must return a data.frame or matrix!")
      ret
    }  
  } else {
    fun.wrapper=function(...) {
      ret = fun(...)
      if (!is.data.frame(ret) & !is.matrix(ret))
        error("run.scenarions: fun(...) must return a data.frame or matrix!")
      ret      
    }
  }
  
  call.list = c(list(FUN=fun.wrapper,MoreArgs=other.par,SIMPLIFY=FALSE),par.grid)
  
  
  ret.list = do.call("mapply",call.list)
  if (show.progress.bar) {
    close(pb)
  }
  # Number of returned rows for each scenario
  len = sapply(ret.list,NROW)
  
  
  par.grid = cbind(data.frame(scen.id=1:NROW(par.grid)),par.grid)
  ind = do.call("c",lapply(1:NROW(par.grid), function(i) rep(i,each=len[i])))
  
  ret = do.call("rbind",ret.list)
  ret = cbind(par.grid[ind,],ret)
  rownames(ret)=NULL
  ret
}

examples.run.scenarios = function() {
  # Compute some values of a linear and quadratic function
  f = function(a,b,c) {
    x = seq(0,1,length=5)
    return(data.frame(x=x,lin=a+b*x,quad=a+b*x+c*x^2))
  }
  ret = run.scenarios(f,par=list(a=c(0,2),b=c(-1,1,2),c=c(0,1,3,4)))
  ret
  
  # Plot using ggplot2
  library(ggplot2)
  qplot(x=x,y=quad,data=ret, facets= a ~ b, color=as.factor(c),geom="line")
  
}

#' A helper function to conduct a simulation study for different parameter combinations
#' @param fun a function that returns a vector, data.frame or matrix
#' @param par an optional list that specifies parameters for different scenarios. fun will be called repl times for each parameter combinantion of the parameters specified in par
#' @param repl for Monte-Carlo simulation the number of times fun is called for each parameter combinantion
#' @param ... additional parameters that will be used by fun
#' @param show.progress.bar shall a progress bar been shown?
#' @param add.run.id shall a column be added that is a unique value for every unique call of fun
#' @param colnames optionally a vector of colnames for the results returned by fun. It is quicker to set colnames just in the end, instead of fun setting names.
#' @param same.seeds.each.par if TRUE (default) then we set for all parameter combinations with which the function is called the same random seed in the i'th replication. One effect e.g. is that if we draw some disturbances eps in our simulation the same disturbances will be drawn in the i'th repitition for all parameter values, we study. This typically facilitates the analysis of comparative statics.
#' @param seeds if same.seeds.each.par = TRUE one can manually provide a vector of random seeds of length repl.
#' @param LAPPLY a function that has the same behavior as lapply. One can use an different function, e.g. in order to parallelize the execution when running a simulation on a computer cluster.
#' @return returns a data.frame that combines the results of all calls to fun and adds the corresponding parameter combinantion and an index for the actual replication. The data.frame can be conviniently analysed graphically, e.g. with ggplot2
#' @export
simulation.study = function(fun, par=NULL, repl=1,..., show.progress.bar = interactive(), add.run.id=TRUE, colnames=NULL,same.seeds.each.par = TRUE,  seeds = floor(runif(repl,0,.Machine$integer.max)),LAPPLY = lapply ) {
  args = list(...)
  
  restore.point("simulation.study")
  
  repl.id = 1:repl
  if (show.progress.bar)
    show.progress.bar = require(R.utils,quietly=TRUE,warn.conflicts=FALSE)
  
  if (same.seeds.each.par) {
    old.seed = get(".Random.seed",.GlobalEnv)
  }
  
  
  repl.fun = function(ri, repl, pi,show.progress.bar, call.par) {
    if (show.progress.bar)
      setTxtProgressBar(pb, ri + (pi-1)*repl)
    if (same.seeds.each.par) {
      set.seed(seeds[ri])
    }
    do.call(fun,c(call.par,args))
  }
  
  
  if (!is.null(par)) {
    par.grid = expand.grid(par)
    par.id = 1:NROW(par.grid)

    if (show.progress.bar)
      pb <- txtProgressBar(min = 1, max = repl*NROW(par.grid), style = 3)
    
    
    all.list = LAPPLY(par.id, function(pi) {
      call.par = as.list(par.grid[pi,])
      names(call.par) = names(par)
      
      res.list = LAPPLY(repl.id, repl.fun, repl=repl, pi=pi, show.progress.bar = show.progress.bar, call.par = call.par)
      
      if (add.run.id) {
        repl.seq = unlist(lapply(1:NROW(res.list), function(i) rep(i,NROW(res.list[[i]]))))
        res.df = cbind(run.id=repl.seq+(pi-1)*repl,do.call(rbind,res.list))
      } else {
        res.df = do.call(rbind,res.list)        
      }
      res.df
    })
    all.par.id = unlist(lapply(1:NROW(all.list), function(i) rep(i,NROW(all.list[[i]]))))
    all.df = cbind(par.id = all.par.id,par.grid[all.par.id,,drop=FALSE],do.call(rbind,all.list))
    if (show.progress.bar)  
      close(pb)
    rownames(all.df)=NULL
  
  # No parameters
  } else {
    if (show.progress.bar)
      pb <- txtProgressBar(min = 1, max = repl, style = 3)
    
    res.list = LAPPLY(repl.id, repl.fun, repl=repl, pi=pi, show.progress.bar = show.progress.bar, call.par = NULL)
    if (add.run.id) {
      repl.seq = unlist(lapply(1:NROW(res.list), function(i) rep(i,NROW(res.list[[i]]))))
      res.df = cbind(run.id=repl.seq,do.call(rbind,res.list))
    } else {
      res.df = do.call(rbind,res.list)        
    }
    rownames(res.df)=NULL
    all.df = res.df
  }
  
  if (same.seeds.each.par) {
    assign(".Random.seed",old.seed,.GlobalEnv)
  }
  
  if (!is.null(colnames)) {
    
    if (!is.null(par)) {
      num.key.cols = NCOL(par.grid)+add.run.id
    } else {
      num.key.cols = 0+add.run.id      
    }
    num.val.cols = NCOL(all.df)-num.key.cols
    if (num.val.cols != length(colnames)) {
      warning(paste("Function fun returns ", num.val.cols, " columns but ", length(colnames), " colnames have been specified."))
    } else {
      colnames(all.df)[(num.key.cols+1):(num.key.cols+num.val.cols)]=colnames
    }
  }
  
  
  return(as.data.frame(all.df))
  
}


simulation.study.with.action.fun = function(fun, par=NULL, repl=1,..., show.progress.bar = interactive(), add.run.id=TRUE, colnames=NULL,same.seeds.each.par = TRUE,  seeds = floor(runif(repl,0,.Machine$integer.max)),LAPPLY = lapply, action.fun=NULL, action.freq=1 ) {
  args = list(...)
  
  restore.point("simulation.study")
  
  repl.chunk = action.freq
  chunks = ceiling(repl / repl.chunk)
  
  if (show.progress.bar)
    show.progress.bar = require(R.utils,quietly=TRUE,warn.conflicts=FALSE)
  
  if (show.progress.bar)
    pb <- txtProgressBar(min = 1, max = chunks, style = 3)
  
  
  sim = NULL
  for (chunk in 1:repl) {
    if (show.progress.bar)
      setTxtProgressBar(pb, chunk)
    
    r.start = (chunks-1) * repl.chunk +1
    r.end = min(r.start + repl.chunk-1, repl)
    
    sim.act = simulation.study(fun,par, repl=repl.chunk, show.progress.bar = FALSE, add.run.id, colnames,same.seeds.each.par,  seeds = seeds[r.start:r.end],LAPPLY,...)
    sim = rbind(sim, sim.act)
    action.fun(sim, ...)
  }
  if (show.progress.bar)  
    close(pb)
  
}


cbind.list.to.df = function(...) {
  df = do.call("c",list(...))
  attr(df, "row.names") <- 1:length(df[[1]])
  attr(df, "class") <- "data.frame"
  df  
}

#' A function that generates a data.frame more quickly from vectors of equal lengths
#' 
#' The function can run substantially quicker than data.frame(...), since no checks will be performed. Yet, the function only works if all vectors have the same length.
#' 
#' @param ... vectors that have equal lengths and will be combined to a data frame
#' @return a data.frame
#' @export
quick.df = function (...) 
{
  df = list(...)
  attr(df, "row.names") <- 1:length(df[[1]])
  attr(df, "class") <- "data.frame"
  df
}

examples.quick.df = function() {
  
  df = quick.df(b=1:5,c=1:5)  
  df
  
  library(rbenchmark)
}

#' Evalutes the expression expr with the specified random seed. Afterwards the function restores the original random seed.
#' 
#' This function can be useful in simulation studies in which some random variables, e.g. explanatory variables X shall always be drawn in the same fashion, while other data, like disturbances epsilon shall differ between runs. One can then draw the X using with.random.seed to guarantee the same X in every simulation run.
#' 
#' @param expr an R expression that returns some random value
#' @param seed an integer number that will be used to call set.seed
#' @return the value of expr evaluated under the specifed random seed
#' @export
with.random.seed = function(expr,seed=1234567890) {
  old.seed = get(".Random.seed",.GlobalEnv)
  set.seed(seed)
  ret = eval(expr)
  assign(".Random.seed",old.seed,.GlobalEnv)
  return(ret)
}

#' Returns the variance-covariance matrix from the return values of a call to lm.fit
#' 
#' Is useful in so far that calling lm.fit is much quicker than calling lm, but the normal vcov function does not work when calling with the result of lm.fit
#' @param lm.fit The return value of a call to lm.fit
#' @return the variance covariance of the ols estimate

vcov.lm.fit = function(lm.fit, qr=lm.fit$qr, residuals=lm.fit$residuals) {
  #    restore.point("vcov")
  p <- qr$rank
  n <-NROW(residuals)
  
  qr.mat <- as.matrix(qr$qr[1L:p, 1L:p])
  qr.mat[row(qr.mat) > col(qr.mat)] <- 0
  
  qrinv <- solve(qr.mat)
  s2 = sum(residuals^2) / (n-p)
  vcov <- qrinv %*% t(qrinv) * s2
  vcov    
}

examples.simulation.study = function() {
  
  # A function that simulates demand
  demand.sim = function(beta0=100,beta1=-1,sigma.eps=0.4,T=200, p.min=0, p.max= (-beta0/beta1)) {  
    # Draw prices always in the same fashion, with a fixed random seed
    p=with.random.seed(runif(T,p.min,p.max), seed=123456789)
    eps = rnorm(T,0,sigma.eps) # Demand Shock for each market
    # Realized demand
    q = beta0 + beta1* p+eps # = beta0 + beta1*p + eps
    #data.frame(p=p,q=q,eps=eps)    
    quick.df(p=p,q=q,eps=eps)       
  }
  # A function that simulates data and performs OLS estimation on the simulated data
  est.sim.fun = function(beta0=100,beta1=-1,...) {
    df = demand.sim(...)
    y = df$q
    X = cbind(1,df$p)
    reg = lm.fit(x=X,y=y)
    quick.df(name=c("beta0.hat","beta1.hat"),coef = coef(reg),true.coef = c(beta0,beta1),se = sqrt(diag(vcov.lm.fit(reg))))
  }
  demand.sim(T=3)
  set.seed(1234)
  est.sim.fun(T=20)

  simulation.study(fun=demand.sim, par=list(T=c(1,2)), repl=2, add.run.id=TRUE)
  
  
  sim = simulation.study(fun=est.sim.fun, par=list(T=c(10,50,100,200), sigma.eps=c(1,3)), repl=50, add.run.id=TRUE, same.seeds.each.par=TRUE)
  
  head(sim)
  
  
  library(ggplot2)  
  
  # Select only the estimate of beta1.hat
  dat = sim[sim$name=="beta0.hat",]
  
  qplot(coef,geom="density",alpha = I(0.6),data=dat, group=T, fill=as.factor(T), facets=sigma.eps~.)  #+ coord_cartesian(ylim = c(0, 75))

  # Compute sample MSE and Bias
  agg = quick.by(dat,list(
        mse = mean((coef-true.coef)^2),
        bias=mean(coef-true.coef)),
        by=c("T","sigma.eps"))
  agg
  
  qplot(y=est.mse,x=T,data=agg,geom="line",group=sigma.eps, color=as.factor(sigma.eps),ylab="Estimated MSE",size=I(1.2))
  qplot(y=est.bias,x=T,data=agg,geom="line",group=sigma.eps, color=as.factor(sigma.eps), ylab="Estimated bias",size=I(1.2))
  
  ret 
}

#' Views variable labels of a data.frame loaded with read.dta
#' @export 
view.stata.var = function(df,View=TRUE,print=!View) {
  stata.var = data.frame(colnames(df),attributes(df)$var.labels)
  colnames(stata.var) = c("name","label")
  View(stata.var)
  if (!print) {
    return(invisible(stata.var))
  }
  return(stata.var)
}



str.to.length = function(str,len,fill=" ") {
  store.objects("str.to.length")
  #restore.objects("str.to.length")
  
  n.max = max(nchar(str))
  
  fill = paste(rep(fill,times=n.max), collapse="")
  substring(paste(str,fill,sep=""),1,len)  
}

# Check whether the first characters of str are equal to table.str
str.start.equal = function(str,table.str, full.str = c("str","table.str")[1],fill=" ") {
  
  stopifnot(length(str)==1)
  if (full.str == "table.str") {
    ncm = nchar(table.str)
    str = str.to.length(str,ncm,fill=fill)
    return(str == table.str & nchar(str) >= ncm)
  } else {
    table.str = str.to.length(table.str,nchar(str),fill=fill)
    return(str == table.str)    
  }
}

example.str.start.equal = function() {
  str = "1234567890"
  table.str = c("123","034","1234567890759375973495739","1")
  str.to.length(table.str,nchar(str),fill=" ")
  str.start.equal(str,table.str)
  str.start.equal(str,table.str, full.str="table.str")
}

# Generate Latex Representation of a Regression
outreg <- function(models, title="Regression", label="", modelLabels=NULL, varLabels=NULL, tight=TRUE, showR2 = TRUE, showRMSE = !TRUE, showAIC=!TRUE, lyx=TRUE,
                   include.groups = list(), include.char = c("yes","","parts"),ignore.var=NULL){
  store.objects("outreg")
  #restore.objects("outreg")
  
  str = ""
  wr = function(...,sep="") {
    str <<- paste(str,...,sep=sep)
  }
  
  modelList <- NULL
  
  ## was input just one model, or a list of models?  ###
  if ( "lm" %in% class(models)) { ##just one model input
    nmodels <- 1
    modelList <- list(modl1=models)
  } else {
    nmodels <- length(models)
    modelList <- models
  } 
  
  ##TODO modelLabels MUST have same number of items as "incoming"
  
  
  ## Get a regression summary object for each fitted model
  summaryList <- list()
  fixnames <- vector()
  myModelClass <- vector()
  
  
  
  i <-  1
  for (model in modelList){
    summaryList[[i]] <- summary(model)
    
    fixnames <- unique( c( fixnames, names(coef(model))))
    myModelClass[i] <- class(model)[1]
    i <- i+1
  }
  
  
  if (length(include.groups)>0) {
    has.group = matrix("?", length(include.groups),length(modelList))
    all.group.var = NULL
    
    for (g in seq_along(include.groups)) {
      group = include.groups[[g]]      
      group.var = lapply(group,function(str) fixnames[str.start.equal(str,fixnames)])
      group.var = unique(unlist(group.var))
      all.group.var = union(all.group.var,group.var)
      
      has.group[g,] = sapply(modelList, function(model) {
        mod.var = names(coef(model))
        ins = intersect(group.var, mod.var)
        if (setequal(ins,group.var)) {
          return(include.char[1]) # yes
        } else if (length(ins)>0) {
          retrun(include.char[3]) # part
        } else {
          return(include.char[2]) # no
        }
      })
    }    
  }
  fixnames = setdiff(fixnames,all.group.var)
  fixnames = setdiff(fixnames,ignore.var)
  
  
  ###If you are just using LaTeX, you need these
  if (lyx == FALSE){
    wr("\\begin{table}\n ")
    wr("\\caption{",title,"}\\label{",label,"}\n ")
  }
  wr("\\begin{center}\n ")
  nColumns <- ifelse(tight, 1+nmodels, 1 + 2*nmodels) 
  wr(paste("\\begin{tabular}{*{",nColumns,"}{l}}\n ", sep=""))
  wr("\\hline\n ")
  
  ### Put model labels on top of each model column, if modelLabels were given
  if (!is.null(modelLabels)){
    wr("     ")
    for (modelLabel in modelLabels){
      if (tight == T) {
        wr(paste("&", modelLabel))
      }else{
        wr(paste("&\\multicolumn{2}{c}{",modelLabel,"}",sep=""))
      }
    }
    wr(" \\\\\n ")
  }
  
  ### Print the headers "Estimate" and "(S.E.)", output depends on tight or other format 
  if (tight == T){
    wr("             ")
    for (i in 1:nmodels) { wr(" & Estimate ") }
    wr(" \\\\\n")
    
    wr("             ")
    for (i in 1:nmodels) {  wr(" & (S.E.) ") }
    wr(" \\\\\n")
  }else{
    
    wr("             ")
    for (i in 1:nmodels) { wr(" & Estimate & S.E.") }
    wr(" \\\\\n")
  }
  
  
  wr("\\hline \n \\hline\n ")
  
  
  print.detailed.var = function(regname) { 
    if ( !is.null(varLabels[[regname]]) ) { 
      wr(paste("",varLabels[[regname]]), sep="")
    }  else {
      wr(paste("", regname), sep="")
    }
    
    for (model in modelList) {
      est <- coef(model)[regname]
      se <- sqrt(diag(vcov(model)))[regname]
      if ( !is.na(est) ) {
        wr(paste("   &   ", round(est,3)))
        pval <- pt(abs(est/se), lower.tail=F, df = model$df.residual)
        if (pval < 0.001/2) {
          wr("***")
        } else if (pval < 0.01/2) {
          wr("**")          
        } else if (pval < 0.05/2) {
          wr("*")          
        }
        
        if (tight == F) {
          wr(paste("   &   (", round(se,3),")",sep=""))
        }
      } else {
        wr("   & . ")
        if (tight == F) wr(" & " )
      }
    }
    wr(" \\\\\n ")
    
    if (tight == T){
      for (model in modelList) {
        est <- coef(model)[regname]
        if (!is.na(est)) wr(paste("   &   (",round(sqrt(diag(vcov(model)))[regname],3)),")",sep="")
        else wr("   &  ")
      }
      wr(" \\\\\n ")
    }    
  }
  
  ### Here come the regression coefficients
  for (regname in fixnames){
    print.detailed.var(regname) 
  }
  
  for (g in seq_along(include.groups)) {
    wr(paste("",names(include.groups)[g]), sep="")
    wr(paste("   &   ", has.group[g,],collapse=""))
    wr(" \\\\\n ")
  }
  
  wr("\\hline \n")
  
  
  ### Print a row for the number of cases
  wr(paste("N"), sep="")
  for (model in summaryList) {
    myDF <- sum( model$df[-3] ) #omit third value from df vector
    wr(paste("   &   ", myDF))
    if (tight == F) wr("    &")
  }
  wr(" \\\\\n ")
  
  
  ### Print a row for the root mean square error
  if ("lm" %in% myModelClass & showRMSE) {
    wr(paste("$RMSE$"),sep="")
    for (model in summaryList) {
      wr( paste("       &", if(is.numeric(model$sigma)) round(model$sigma,3)))
      if (tight == F) wr("    &")
    }
    wr("  \\\\\n ")
  }
  
  
  ### Print a row for the R-square
  if ("lm" %in% myModelClass & showR2) {
    wr(paste("$R^2$"),sep="")
    for (model in summaryList) {
      wr( paste("       &", if(is.numeric(model$r.square))round(model$r.square,3)))
      if (tight == F) wr("    &")
    }
    wr("  \\\\\n ")
  }
  
  
  ## Print a row for the model residual deviance
  if ("glm" %in% myModelClass) {
    wr(paste("$Deviance$"),sep="")
    for (model in summaryList) {
      wr(paste("      &", if(is.numeric(model$deviance))round(model$deviance,3)))
      if (tight == F) wr("      &")
    }
    wr("  \\\\\n ")
  }
  
  ### Print a row for the model's fit, as -2LLR
  if ("glm" %in% myModelClass) {    
    wr(paste("$-2LLR (Model \\chi^2)$"),sep="")
    for (model in modelList) {
      if (is.numeric(model$deviance)){
        n2llr <- model$null.deviance - model$deviance 
        wr(paste("      &", round(n2llr,3)))
        gmdf <- model$df.null - model$df.residual + 1
        
        if (pchisq(n2llr, df= gmdf, lower.tail=F) < 0.05) {wr("*")}
      }
      
      else {
        wr("    &")
      }
      if (tight == F) wr("      &")
    }
    wr("  \\\\\n ")
  }
  
  
  
  ## Print a row for the model's fit, as -2 LLR
  ### Can't remember why I was multiplying by -2
  
  if (showAIC == T) {
    wr(paste("$AIC$"),sep="")
    for (model in modelList) {
      wr(paste("      &", if(is.numeric(AIC(model)))round(AIC(model),3)))
      if (tight == F) wr("      &")
    }
    wr("  \\\\\n ")
  }
  
  
  
  wr("\\hline\\hline\n")
  num.col = length(modelList)+1
  wr("\\multicolumn{",num.col,"}{l}{(Std. Errors), * $p \\le 0.05$, ** $p \\le 0.01$, *** $p \\le 0.001$}",sep="")
  wr("\\end{tabular}\n")
  wr("\\end{center}\n")
  if (lyx == FALSE){ 
    wr("\\end{table}\n")
  }
  
  writeClipboard(str)
  cat(str)
  #cat("Written to clipboard...")
}


# Adds a column to a data.table given by expr
# The column is added after the keys indicated with by
add.by.col = function(dt,name,expr,by) {
  if (!is.character(expr))
    expr = deparse(substitute(expr))
  by.str = paste('c("',paste(by,collapse='","'),'")',sep="")
  code = paste('dt[,list("',name,'"=',expr,'),keyby=',by.str,"]",sep="")
  new.dt = eval(base::parse(text=code))
  new.dt[dt,]
}

#' Quick version of by using internally data.table

#' @author Sebastian Kranz
#' @param df a data.frame that shall be aggregated / transformed
#' @param by either a character vector with the columns of df over which grouping shall take place or a list vectors of size NROW(df) containing group indices
#' @param expr a string containing an R expr operating on columns in df that shall be evaluated, corresponds to j parameter in data.table, can be a named list to generate multiple columns (see data.table introduction). 
#' @param add.col if TRUE the data generated by group-wise evaluation of expr will be cbinded to the corresponding rows in df
#' @return a data.frame with values of indices in by and returning expressions
#' @export
quick.by = function(df,by=NULL,expr,add.col=FALSE, as.data.table=FALSE,keep.row.order=add.col,...) {
  require(data.table)

  # Convert expr to str.expr
  is.char = tryCatch(is.character(expr), error = function(e) FALSE)
  if (!is.char) {
    str.expr = paste(deparse(substitute(expr)),collapse="")
  } else {
    str.expr = expr
  }
  
  if (length(str.expr)==0) {
    stop(paste('No expr provided!\n Note: expressions like x=sum(z), have to be provided as strings, i.e. "x=sum(z)".'))
  }
  args = list(...)
  copy.into.env(source=args)
  expr = NULL
  restore.point("quick.by")
  
  
  str.expr = gsub(".SUBDATA","as.data.frame(.SD)",str.expr,fixed =TRUE)[[1]] 
  
  # str.expr = remove.list.from.str.expr(str.expr)
  # Possibly add index columns
  dt = as.data.table(df)
  
  # Don't group
  if (is.null(by)) {
    if (add.col) {
      lhsrhs = lhsrhs.str.expr(str.expr)
      code = paste('dt[,',lhsrhs[,1],':=',lhsrhs[,2],"]",sep="")
    }
    code = paste('dt[,',str.expr,"]",sep="")
    dt = eval(base::parse(text=code))
    if (as.data.table)
      return(dt)
    return(as.data.frame(dt))
  }
   
  if (is.list(by)) {
    by.names = names(by)
    new.names = setdiff(by.names,colnames(df))
    if (length(new.names)>0) {
      dt[,new.names := by[new.names],with=FALSE]
    }
    by=by.names
  }
  
  if (keep.row.order) {
    if (!add.col) {
      stop("keep.row.order can be set to TRUE only if add.col == TRUE, otherwise the command does not make sense")
    }
    row.id.name = paste(".ROW.ID.",round(runif(1,1000,100000000)),sep="")
    dt[,row.id.name := 1:NROW(dt), with=FALSE]
  }
  
  # Sorts dt
  setkeyv(dt,by)          
  
  str.expr = add.list.to.str.expr.list(str.expr)
  #str.expr = paste("list(",str.expr,")")
  by.str = paste('c("',paste(by,collapse='","'),'")',sep="")
  code = paste('dt[,',str.expr,',by=',by.str,"]",sep="")
  
  #dt[,compute.u.ij(p,xi,x=hp,v.i=v.i),by="j"]
  
  #print(code)
  #code = "dt[,compute.prices.logit(X=list(hp,li),xi=xi,cost=cost) ,by=c(\"t\")]" 
  new.dt = eval(base::parse(text=code))
  
  if (add.col) {    
    # remove existing cols with same name as the created ones
    existing.cols = intersect(colnames(dt), setdiff(colnames(new.dt),by))
    for (col in existing.cols)
      dt[,col:=NULL,with=FALSE]
    
    for (old.col in intersect(by,colnames(new.dt)))
    # Can simply add columns
    if (NROW(new.dt)==NROW(dt)) {
      new.cols = setdiff(colnames(new.dt),colnames(dt))
      if (length(new.cols)>0)
        ret.dt = cbind(dt,new.dt[,new.cols,with=FALSE])
    # Match columns via keys
    } else {
      setkeyv(new.dt,by)
      ret.dt = new.dt[dt,]
      colnames=colnames(new.dt)[-(1:length(by))]
      setcolorder(ret.dt,unique(c(colnames(dt),colnames)))
    }
    
    if (keep.row.order) {
      # Restore original row order by sorting according to row.id.name
      setkeyv(ret.dt,row.id.name)
      # Remove the column row.id.name
      ret.dt[,row.id.name:=NULL,with=FALSE]
    }
    
    if (as.data.table)
      return(ret.dt)
    return(as.data.frame(ret.dt))
  }  
  if (as.data.table)
    return(new.dt)
  as.data.frame(new.dt)
}


examples.quick.by = function() {
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
}

add.list.to.str.expr.list = function(str.expr) {
  vec = strsplit(str.expr,"")[[1]]
  depth = cumsum(vec=="(") - cumsum(vec==")")
  sep.pos = which(vec %in% c(",","=") & depth==0)
  if (length(sep.pos)>0) {
    return(paste("list(",str.expr,")"))
  }
  return(str.expr)
}

remove.list.from.str.expr = function(str.expr) {
  library(stringr)
  str.expr = str_trim(str.expr)
  if (substring(str.expr,1,4)!="list")
    return(str.expr)
  after.list = str_trim(substring(str.expr,5))
  if (substring(after.list,1,1)!="(")
    return(str.expr)
  
  return(substring(after.list,2,nchar(after.list)-1))
}

examples.remove.list.from.str.expr = function() {
  remove.list.from.str.expr(' list (a=5,b = "Hi how are you?")')
  remove.list.from.str.expr('(a=5,b = "Hi how are you?")')
  
}

#' Substitute some pattern by a list of alternatives
#' @export
subst.expr = function(expr.str,subst,collapse=",", add.list=FALSE) {
  restore.point("subst.expr")
  code = expr.str
  res.code = list()
  counter = 0
  for (i in seq_along(subst)) {
    pattern     = names(subst)[i]
    res.code = lapply(subst[[i]], function(new) {
      gsub(pattern,new,code,fixed=TRUE)
    })
    code = paste(res.code,collapse=collapse)
  }
  if (add.list)
    code = paste("list(",code,")")
  return(code)
}
examples.subst.expr = function() {
  subst.expr("mean.ATTR=mean(ATTR)",subst=list(ATTR=c("hp","li")))
}

#quick.by = cmpfun(quick.by)


expr.to.string = function(expr) {
  str = deparse(substitute(expr))
  if (substr(str,1,1)=='"')
    str = substring(str,2,nchar(str)-1)
  str
}

lhsrhs.str.expr = function(str.expr) {
  library(stringr)
  str.expr = paste(str.expr,collapse="")
  str.expr = remove.list.from.str.expr(str.expr)
  
  vec = strsplit(str.expr,"")[[1]]
  depth = cumsum(vec=="(") - cumsum(vec==")")
  sep.pos = which(vec %in% c(",","=") & depth==0)

  tokens = str_trim( substring(str.expr,c(0,sep.pos)+1,c(sep.pos,nchar(str.expr)+1)-1))
  
  matrix(tokens,ncol=2,byrow=TRUE)
}  
examples.lhsrhs.str.expr = function() {
  str.expr = c("x = f(x=4,y=5), z=23")
  lhsrhs.str.expr(str.expr)
}

cross.paste = function(left,right,sep="_") {
  as.character(t(outer(left,right,paste,sep=sep)))  
}
paste0 = function(...,sep="") {
  paste(...,sep=sep)
}

# A variant of the colsplit function in plyr that works much faster for large vectors with many duplicates. 
colsplit = function (string, pattern, names,split.unique = NROW(string)>100) 
{
  # Original Computation: split all string
  # Problem str_split_fixed can be quite slow for long vectors
  if (!split.unique) {    
    vars <- str_split_fixed(string, pattern, n = length(names))
    df <- data.frame(alply(vars, 2, type.convert, as.is = TRUE), 
                     stringsAsFactors = FALSE)
    names(df) <- names
    
    # Only split unique strings and match afterwards
    # works much faster for long vectors with many duplicates
  } else {
    uni.string = unique(string)
    # Only have speed gains in case there are substantially less
    # unique strings than normal strings
    if (length(uni.string)>0.5*length(string))
      return(colsplit(string,pattern,names,split.unique = FALSE))
    uni.df <- colsplit(uni.string,pattern,names,split.unique=FALSE)
    rows <- match(string,uni.string)
    df <- uni.df[rows,]
  }
  df
}

#' Remove all global variables (not functions)
#' @export
remove.global.variables = function(exclude=NULL,envir = .GlobalEnv) {
  var = setdiff(ls(envir=envir), lsf.str(envir=envir))
  #print(var)
  var = setdiff(var,exclude)
  if (length(var)>0) {
    rm(list = var, envir = envir, inherits = FALSE)
    message("removed ", paste(var,collapse=","))
  } else {
    #message("there were no global variables")
  }
}
#remove.global.variables()


#' Creates a list that is named by the names of its arguments
#' @export
nlist = function(...) {
  li = list(...)
  li.names = names(li)
  
  names = unlist(as.list(match.call())[-1])
  if (!is.null(li.names)) {
    no.names = li.names == ""
    names(li)[no.names] = names[no.names]
  } else {
    names(li) = names
  }
  li
}

#' Creates a vector that is named by the names of its arguments
#' @export
nc = function(...) {
  #restore.point("nc")
  li = c(...)
  li.names = names(li)  
  names = unlist(as.list(match.call())[-1])
  if (!is.null(li.names)) {
    no.names = li.names == ""
    names(li)[no.names] = names[no.names]
  } else {
    names(li) = names
  }
  li
}


#' Returns estimated coefficients and standard errors from a regression model. One can also specify linear or non-linear transformations of the original coefficients, standard errors are then computed using the delta method.
#' @param reg results of any estimation (like lm) that has methods coef(reg) and vcov(reg)
#' @param trans.coef a list 
#' @export
get.coef.and.se = function(reg,trans.coef=NULL, coef. = coef(reg), vcov. = vcov(reg)) {
  restore.point("get.coef.and.se")
  df = cbind(coef.,sqrt(diag(vcov.)))
  
  if (!is.null(trans.coef)) {
    ret = lapply(trans.coef,function(g) as.numeric(deltaMethod(coef.,g,vcov.=vcov.)))
    ret = do.call(rbind,ret) 
    df = rbind(df,ret)
  }
  colnames(df) = c("coef","se")
  df = as.data.frame(df)
  df
}

examples.get.coef.and.se = function() {
  get.coef.and.se(m1,trans.coef = list(ratio="t1/t2",prod="t1*t2",inv_t1="1/t1"))
  
  coef(m1)["(Intercept)"]
  se(m1)
  ?coef
  
  deltaMethod(m1, "b1+b2", parameterNames= paste("b", 0:2, sep="")) 
  deltaMethod(m1, "t1/t2") # use names of preds. rather than coefs.
  deltaMethod(m1, "t1/t2", vcov=hccm) # use hccm function to est. vars.
  # to get the SE of 1/intercept, rename coefficients
  deltaMethod(m1, "1/b0", parameterNames= paste("b", 0:2, sep=""))
  # The next example calls the default method by extracting the
  # vector of estimates and covariance matrix explicitly
  deltaMethod(coef(m1), "t1/t2", vcov.=vcov(m1))
}


#' Convenience interface to gvisMotionChart that transforms timevar into a date and allows to set default columns
#' @export
skMotionChart = function(df,idvar=colnames(df)[1],timevar=NULL,xvar=NULL,yvar=NULL, colorvar=idvar, sizevar = NULL,...) {
  restore.point("skMotionChart")
  require(googleVis)
  
  # Generate a constant variable as column for time if not provided
  # Unfortunately the motion plot still shows 1900...
  if (is.null(timevar)) {
    .TIME.VAR = rep(0,NROW(df))
    df = cbind(df,.TIME.VAR)
    timevar=".TIME.VAR"
  }
  # Transform numeric timevar into single years
  if (is.numeric(df[[timevar]])) {
    df[[timevar]] <- as.Date(paste(df[[timevar]],"-01-01",sep=""),format="%Y-%m-%d")
  }
  # Transform booleans into 0 and 1 since otherwise an error will be thrown
  for (i in  1:NCOL(df)) {
    if (is.logical(df [,i])[1])
      df[,i] = df[,i]*1
  }
  
  # Rearrange columns in order to have the desired default values for
  # xvar, yvar, colorvar and sizevar
  firstcols = c(idvar,timevar,xvar,yvar,colorvar,sizevar)
  colorder = c(firstcols, setdiff(colnames(df),firstcols))
  df = df[,colorder]
  
  gvisMotionChart(df,idvar=idvar,timevar=timevar,...)
}



# Sort a data frame or matrix
# by one or more of its columns
# 
# param x the data frame or matrix
# param cols vector of columns (names or indices)
# param decreasing logical vector indicating for each column whether it shall be sorted in a decreasing fashion or not
# param na.last how should NA values be ordered (see sort or order) 
sort.by.col = function(x,cols=1,decreasing=FALSE, na.last=TRUE,...) {
  #restore.point("sort.by.col")
  if (is.character(decreasing)) {
    decr.cols = decreasing
    decreasing = rep(FALSE,length(cols))
    if (is.character(cols)) {
      names(decreasing) = cols
    } else {
      names(decreasing) = colnames(x)[cols]
    }
    decreasing[decr.cols] = TRUE
  } else {
    decreasing = rep(decreasing, length=length(cols))
    if (is.character(cols)) {
      names(decreasing) = cols
    }    
  }
  col.list = lapply(cols, function(col) x[,col]*(-1)^decreasing[col])
  names(col.list) = NULL
  ord = do.call(order,c(col.list,list(decreasing=FALSE,na.last=na.last)))
  x[ord,]
}

examples.sort.by.col = function() {
  x = data.frame(a=c(3,2,1,1),b=c(2,2,1,3),c=c(1,2,2,3))
  # Sort by the second column
  sort.by.col(x,2)
  # Same result, but less intuitive to read
  x[order(x[,2]),]
  
  # Sort decreasingly  
  sort.by.col(x,2,decreasing=TRUE)  
  
  # Sort first by column "a" and then by "c"
  sort.by.col(x,c("a","c"))

  # 2 different ways to specify that "c" shall be sorted decreasingly
  sort.by.col(x,c("a","c"),decreasing="c")
  sort.by.col(x,c("a","c"),decreasing=c(FALSE,TRUE))
}

#' Uses the functions texreg and htmlreg in the package texreg to show regression results of several models next to each other. If called from the console uses screenreg otherwise uses by default htmlreg
showreg = function(l,custom.model.names=NULL,console=interactive(), interactive.fun=screenreg,noninteractive.fun = htmlreg,doctype=FALSE,...) {
  #restore.point("showreg",to.global=FALSE)
  if (!is.null(names(l)) & is.null(custom.model.names))
    custom.model.names=names(l)
  if (console) {
    return(interactive.fun(l,custom.model.names=custom.model.names,...))
  } else {
    return(noninteractive.fun(l,custom.model.names=custom.model.names,doctype=doctype,...))    
  }
}




ivreg2 <- function(form,endog,iv,data,digits=3){
  # library(MASS)
  # model setup
  restore.point("ivreg2")
  
  # Estimate OLS
  r1 <- lm(form,data)
  
  # Get dependend variable 
  y <- r1$fitted.values+r1$resid
  # Matrix of explanatory variables
  x <- model.matrix(r1)
  
  
  # Get matrix of instruments & explanatory variables
  aa <- rbind(endog == colnames(x),1:NCOL(x)) 
  z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv]) 
  colnames(z)[(dim(z)[2]-length(iv)+1):(dim(z)[2])] <- iv 
  
  
  # iv coefficients and standard errors
  z <- as.matrix(z)
  pz <- z %*% (solve(crossprod(z))) %*% t(z)
  biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
  sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
  vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
  res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
  res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res) <- colnames(x)
  colnames(res) <- c("Coef","S.E.","t-stat","p-val")
  
  # First-stage F-test
  y1 <- data[,endog]
  z1 <- x[,aa[2,aa[1,]==0]]
  bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
  bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
  rss1 <- sum((y1 - z %*% bet1)^2)
  rss2 <- sum((y1 - z1 %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  firststage <- c(fs)
  firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
  colnames(firststage) <- c("First Stage F-test")
  
  # Hausman tests
  bols <- solve(crossprod(x)) %*% crossprod(x,y)
  sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
  vbols <- as.numeric(sigols)*solve(crossprod(x))
  sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
  x1 <- x[,!(colnames(x) %in% "(Intercept)")]
  z1 <- z[,!(colnames(z) %in% "(Intercept)")]
  pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
  biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
  bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
  # Durbin-Wu-Hausman chi-sq test:
  # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
  # hpvl <- 1-pchisq(haus,df=1)
  # Wu-Hausman F test
  resids <- NULL
  resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
  x2 <- cbind(x,resids)
  bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
  bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
  rss1 <- sum((y - x2 %*% bet1)^2)
  rss2 <- sum((y - x %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  fpval <- 1-pf(fs, p1-p2, n1-p1)
  #hawu <- c(haus,hpvl,fs,fpval)
  hawu <- c(fs,fpval)
  hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
  #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
  colnames(hawu) <- c("Wu-Hausman F-test","p-val") 
  
  # Sargan Over-id test
  ivres <- y - (x %*% biv)
  oid <- solve(crossprod(z)) %*% crossprod(z,ivres)
  sstot <- sum((ivres-mean(ivres))^2)
  sserr <- sum((ivres - (z %*% oid))^2)
  rsq <- 1-(sserr/sstot)
  sargan <- length(ivres)*rsq
  spval <- 1-pchisq(sargan,df=length(iv)-1)
  overid <- c(sargan,spval)
  overid <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),overid)),ncol=length(overid))
  colnames(overid) <- c("Sargan test of over-identifying restrictions","p-val")
  if(length(iv)-1==0){
    overid <- t(matrix(c("No test performed. Model is just identified")))
    colnames(overid) <- c("Sargan test of over-identifying restrictions")
  }
  full <- list(results=res, weakidtest=firststage, endogeneity=hawu, overid=overid)
  return(full)
}

#' Computes robust variance covariance estimator of a model
#' works at least with lm (OLS) 
#' Goal: Make it work correctly for ivreg (instrumental variables)
#'  
#' Adapted from code by Drew Dimmery
#' http://www.drewdimmery.com/robust-ses-in-r/
robust.vcov <- function(model, type=ifelse(is.null(cluster),"HC1","cluster"),cluster=NULL,...){
  require(sandwich)
  
  
  if (type=="HAC") {
    vcov = vcovHAC(model,...)
  } else if (type == "cluster") {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
    cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  } else {
    vcov = vcovHC(model,type)
  }
  return(vcov)
}

examples.ivreg2 = function() {
  
  T = 100
  eps = rnorm(T,0,4)
  wages = runif(T,10,20)
  material = runif(T,0,10)
  c = wages+material
  
  beta0 = 100
  beta1 = -1
  
  a = beta0 + eps; b=-beta1
  # Optimal prices
  p = (a + b*c) / (2*b)
  q = beta0 + beta1*p + eps
  
  dat = data.frame(p=p,q=q,wages=wages,material=material)
  
  ivreg2(form=q ~ p,endog="p",iv=c("wages","material"),data=dat)
  
  library(AER)
  iv = ivreg(q~p|wages+material)
  robust.vcov(iv,type="HC0")
  robust.vcov(iv,type="HAC")
 
  vcovHC(iv)
  sandwich(iv, bread=bread.ivreg, meat=meatHC)
  
  vcov(iv)
  iv$terms
}