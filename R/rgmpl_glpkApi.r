# Hilfsfunktionen um GMPL Modelle in R zu lösen
#
# Autor: Sebastian Kranz (skranz@uni-bonn.de)
#
# Version vom 07.12.2011

library(glpkAPI)
library(stringr)




#' Gets a list with sets, variables and parameters
#' of a GMPL model in the file mod.file
#' @export
gmpl.get.model.info = function(mod.file) {
  
  
  # Gets a list with sets, variables and parameters
  # of a GMPL model in the file mod.file
  
  require(stringr)
  str = readLines(mod.file)
  # Remove whitespaces from left or right 
  str = str_trim(str)
  
  # Find sets 
  rows = str_sub(str,1,3) == "set"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  sets.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each set
  brace.start = str_locate(txt,fixed("{"))[,1]
  brace.end = str_locate(txt,fixed("}"))[,1]
  in.brace = str_sub(txt,brace.start+1,brace.end-1)
  sets.sets = str_split(in.brace,fixed(','))
  sets.sets = lapply(sets.sets,str_trim)
  names(sets.sets)=sets.name

  
  # Find variables
  rows = str_sub(str,1,3) == "var"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  var.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each variable
  equal.start = str_locate(txt,fixed("="))[,1]
  brace.start = str_locate(txt,fixed("{"))[,1]
  brace.end = str_locate(txt,fixed("}"))[,1]
  in.brace = str_sub(txt,brace.start+1,brace.end-1)
  var.sets = str_split(in.brace,fixed(','))
  var.sets = lapply(var.sets,str_trim)
  var.sets[!is.na(equal.start) & equal.start<brace.start] = NA
  
  names(var.sets)=var.name
  
  
  # Find parameters
  rows = str_sub(str,1,5) == "param"
  txt = str_trim(str_sub(str[rows],6))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  param.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each variable
  equal.start = str_locate(txt,fixed("="))[,1]
  brace.start = str_locate(txt,fixed("{"))[,1]
  brace.end = str_locate(txt,fixed("}"))[,1]
  in.brace = str_sub(txt,brace.start+1,brace.end-1)
  param.sets = str_split(in.brace,fixed(','))
  param.sets = lapply(param.sets,str_trim)
  param.sets[!is.na(equal.start) & equal.start<brace.start] = NA
  
  names(param.sets)=param.name
  
  return(list(sets=sets.name,sets.sets = sets.sets,var=var.name,var.sets=var.sets,
              param=param.name,param.sets=param.sets))
}  

#' Generates a GMPL data file
#' 
#' Generates a GMPL data file for the model specified in dat.file
#' sets and param are lists that contain the 
#' values of the sets and parameters that
#' are specified in the GMPL model
#' @param sets a list with the sets used by the gmpl model
#' @param param a list with the parameters used by the gmpl model
#' @param mod.file path of the .mod file in which the gmpl model is specified
#' @param dat.file path of the .dat file in which the data shall be written
#' @export
gmpl.make.dat.file =  function(sets=NULL,param=NULL,mod.file,dat.file) {  
  mi = gmpl.get.model.info(mod.file)
  
  if (!setequal(names(sets),mi$sets)) {
    message("Error: Model file specifies the following sets:")
    print(mi$sets)
    message("but you specified now the following sets:")
    print(names(sets))
    stop()
  }
  if (!setequal(names(param),mi$param)) {
    message("Error: Model file specifies the following parameters:")
    print(mi$param)
    message("but you specified now the following parameters:")
    print(names(param))
    stop()
  }
  
  mypaste = function(..., sep="",collapse=" ") {
    paste(...,sep=sep,collapse=collapse)
  }
  #########################################
  # write sets
  #########################################
  
  if (!is.null(sets)) {
    str = character(length(sets))
    for (i in 1:length(sets)) {
      # Set is not defined over another set
      if (is.na(mi$sets.sets[[i]])) {
        str[i] = mypaste(
          "  set ",names(sets)[i], " := ", mypaste(sets[[i]]),";\n")
      
      # Set is defined over one or more sets
      # Assume that set is a list in the right order
      } else {
        set = sets[[i]]
        set.name = names(sets)[[i]]
        set.sets = sets[mi$sets.sets[[set.name]]]
      
        
        if (length(set.sets)==1) {
          index.name = set.sets[[1]]
        } else { 
          grid = expand.grid(set.sets)
          index.name = grid[,1]
          for (j in 2:NCOL(grid)) {
            index.name = paste(index.name,grid[,j],sep=",")
          }
        }
        
        if (!is.list(set) | length(set) != NROW(index.name)) {
          stop(paste("Error: Model file specifies set ",set.name," as a set over the sets ",paste(set.sets,collapse=","),". You must therefore specify this set as a list of sets with length ", NROW(index.name), "!"))
        }          
        fun = function(ind) {
          mypaste("  set ",set.name, "[",index.name[ind],"] := ", mypaste(set[[ind]]),";\n")  
        }
        str[i] = paste(sapply(1:NROW(index.name),fun),collapse="\n")
      }
    }
    
    str = mypaste(
"
/* Sets */
",
    mypaste(str,collapse="\n"))
  }
    
  txt = str
  # write parameters
  if (!is.null(param)) {
    str = character(length(param))
    for (i in 1:length(param)) {
      p = param[[i]]
      pn = names(param)[[i]]
      pset = sets[mi$param.sets[[pn]]]
   
      if (is.na(mi$param.sets[[pn]])[1]) {
         if (length(p)>1) {
          stop(paste("Error: Model file specifies parameter",pn," as a single number, but you provided a vector: ",pn, "=",paste(p,collapse=" ")))
        }
      } else if (length(pset)==1) {
        if (length(p) != length(pset[[1]])) {
          stop(paste("Error: Parameter ",pn," is defined over set ", paste(names(pset),collapse=" "), " and should have ", length(pset[[1]]), " elements, but the argument has ", length(p), "elements"))
        }
      } else if (length(pset)==2) {
        if (!is.matrix(p)) {
          stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you did not provide a matrix."))          
        }
        if (NROW(p)!=length(pset[[1]]) | NCOL(p)!=length(pset[[2]])) {
          stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you provided a ",NROW(p), "x", NCOL(p), " matrix."))          
        }
      } else if (length(pset>=3)) {
        stop(paste("Error: Parameter ",pn," is defined over more than 2 sets: ", names(pset), ". Unfortunately, this function works so far only for at most two dimensional parameters."))          
      }

      # A matrix
      if (is.matrix(p)) {
        tstr = apply(p,1,function (row) paste(row,collapse=" "))
        rowset = pset[[1]]
        colset = pset[[2]]
        tstr = paste("      ",rowset,"  ",tstr,collapse="\n",sep="")
        tstr = mypaste(" param ",pn,": ",paste(colset,collapse=" "),":= \n",
                                         tstr, ";\n")
        
        str[i] = tstr  
      # A parameter defined over a single set
      } else if (!is.na(mi$param.sets[[pn]])[1]) {        
        str[i] = mypaste("  param ",pn, " := ", mypaste("[",pset[[1]],"] ",p),";\n")        
      # A single number
      } else {
        str[i] = mypaste("  param ",pn, " := ", p,";\n")
      }
    }
    str = mypaste(
"
/* Parameter */
",
    mypaste(str,collapse="\n"))
  }
  txt = paste(txt,str,sep="\n")

  if (!is.null(dat.file)) {
    writeLines(txt,dat.file)
  }
  #return(txt)
}

#' Load a GMPL model and data and generate a GLPK object
#' @export
gmpl.load.problem = function(mod.file,dat.file) {
  require(glpkAPI)
  wk  = mplAllocWkspGLPK()
  mplReadModelGLPK(wk,mod.file,skip=0)
  mplReadDataGLPK(wk,dat.file)
  mplGenerateGLPK(wk, fname = NULL)
  
  lp <- initProbGLPK()
  mplBuildProbGLPK(wk, lp)
  mplFreeWkspGLPK(wk)
  lp
}

#' Solve a GMPL problem using glpkAPI
#' @param mod.file path of the .mod file in which the gmpl model is specified
#' @param dat.file path of the .dat file in which the gmpl data is specified
#' @param lp optional a link to the GLPK problem generated by gmpl.load.problem
#' @param delete.lp default = TRUE if lp is given, shall it be removed from memory after it has been solved?
#' @param adapt.sol default = TRUE shall the solution be returned in a more convenient form
#' @export 
gmpl.solve = function(mod.file=NULL,dat.file=NULL,lp=NULL,
                      delete.lp = is.null(lp),adapt.sol=TRUE) {
  # Solves a GMPL model with a given dat.file
  # and model.file
  
  if (is.null(lp)) {
    message("Load problem...")
    lp = gmpl.load.problem(mod.file,dat.file)
  }
  message("Solve problem...")

  res = glpk.solve(lp,delete.lp)
  
  if (adapt.sol) {
    res$sol = gmpl.adapt.sol(res$sol,mod.file=mod.file)
  }
  return(res)
}

#' Solve a GLPK linear problem
#' 
#' @param lp a GLPK problem generated e.g. by a call to gmpl.load.problem
#' @param delete.lp default = TRUE shall the problem lp be removed from memory after it has been solved?
#' @export
glpk.solve = function(lp=NULL, delete.lp = TRUE) {
  # solve model with simplex algorithm
  #require(glpkAPI)
  
  
  message("Solve model...")
  code=solveSimplexGLPK(lp)
  message("Retrieve solution (can be slow...)")
  # Retrieve Solution
  nc = getNumColsGLPK(lp)
  nr = getNumRowsGLPK(lp)
  
  #nc = glp_get_num_cols(lp)
  #nr = glp_get_num_rows(lp)
  
  #val = glp_get_obj_val(lp)
  val = getObjValGLPK(lp)
  #sol           = mapply(glp_get_col_prim,j=1:nc,MoreArgs=list(lp=lp))
  sol           = mapply(getColPrimGLPK,j=1:nc,MoreArgs=list(lp=lp))
  #sol.name      = mapply(glp_get_col_name,j=1:nc,MoreArgs=list(lp=lp))
  sol.name      = mapply(getColNameGLPK,j=1:nc,MoreArgs=list(lp=lp))
  
  names(sol)    = sol.name
  
  shadow.prices = mapply(getRowDualGLPK,i=1:nr,MoreArgs=list(lp=lp))
  constr.name   = mapply(getRowNameGLPK,i=1:nr,MoreArgs=list(lp=lp))
  names(shadow.prices) = constr.name
  
  # Delete the linear program in order to free memory
  if (delete.lp) {
    delProbGLPK(lp)
    #glp_delete_prob(lp)
  }
  
  return(list(code=code,val=val,sol=sol,shadow.prices=shadow.prices))
}

# Internal function write solution in a nicer form
gmpl.adapt.sol = function(sol,mod.file=NULL,mi=NULL) {
  lab = names(sol)
  left.bracket.pos = str_locate(lab,fixed("["))[,1]  
  var = str_sub(lab,start=1,end=left.bracket.pos-1)
  arg.str = str_sub(lab,start=left.bracket.pos+1,end=-2)
  
  if (!is.null(mod.file)) {
    mi = gmpl.get.model.info(mod.file)
  }
  
  ret = list()
  for (vn in unique(var)) {
    rows = which(var == vn)
    num.comma = NROW(str_locate_all(arg.str[rows[1]],fixed(","))[[1]])
    df = as.data.frame(matrix(NA,NROW(rows),num.comma+2))
    act.arg.str = arg.str[rows]
    if (num.comma>0) {
      for (i in 1:num.comma) {
        comma.pos = str_locate(act.arg.str,fixed(","))[,1]
        df[,i] =  str_sub(act.arg.str,1,comma.pos-1)
        act.arg.str = str_sub(act.arg.str,start=comma.pos+1,end=-1)
      }
    }
    df[,num.comma+1] = act.arg.str
    df[,num.comma+2] = sol[rows]
    if (!is.null(mi)) {
      colnames(df)=c(mi$var.sets[[vn]],vn)
    } else {
      colnames(df)=c(paste("arg",1:(num.comma+1),sep=""),vn)
    }
    ret[[vn]]=df
  }
  return(ret)
} 
