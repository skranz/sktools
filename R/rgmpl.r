# Hilfsfunktionen um GMPL Modelle in R zu lösen
#
# Autor: Sebastian Kranz (skranz@uni-bonn.de)
#
# Version vom 07.12.2011

library(glpkAPI)
library(stringr)


#' Paste together columns of a matrix or data.frame
#' @export
paste.matrix.cols = function(mat,cols=1:NCOL(mat),...) {
  if (NROW(cols)==2) {
    return(paste(mat[,cols[1]],mat[,cols[2]],...))
  } else if (NROW(cols)==3) {
    return(paste(mat[,cols[1]],mat[,cols[2]],mat[,cols[3]],...))
  } else {
    code = paste("mat[,",cols,"]",collapse=",")
    code = paste("paste(",code,",...)",sep="")
    return(eval(parse(text=code)))
  }
}


ADAPT.SOL = FALSE

#' Gets a list with sets, variables and parameters
#' of a GMPL model in the file mod.file
#' @export
gmpl.get.model.info = function(mod.file) {
  
  restore.point("gmpl.get.model.info")
  
  require(stringr)
  str = readLines(mod.file)
  # Remove whitespaces from left or right 
  str = str_trim(str)
  
  # Find sets 
  rows = str_sub(str,1,3) == "set"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  sets.name = str_sub(txt,1,end.name-1)
  
  extract.sets = function(txt) {
    brace.start = str_locate(txt,fixed("{"))[,1]
    brace.end = str_locate(txt,fixed("}"))[,1]
    in.brace = str_sub(txt,brace.start+1,brace.end-1)
    my.sets = str_split(in.brace,fixed(','))
    my.sets = lapply(my.sets,str_trim)
    my.sets = lapply(my.sets, function(set) {
      if (is.na(set[1]))
        return(set)
      set = str_trim(set)
      in.pos = str_locate(set,fixed(" in "))[,2]
      rows = which(!is.na(in.pos))
      if (length(rows)>0)
        set[rows] = substring(set[rows],in.pos[rows]+1)                
      return(set)
    })
    return(my.sets)
  }
  
  # Find sets for each set
  sets.sets = extract.sets(txt)
  names(sets.sets)=sets.name

  
  # Find variables
  rows = str_sub(str,1,3) == "var"
  txt = str_trim(str_sub(str[rows],4))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  var.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each variable
  var.sets = extract.sets(txt)
  names(var.sets)=var.name
  
  # Find parameters
  rows = str_sub(str,1,5) == "param"
  txt = str_trim(str_sub(str[rows],6))
  end.name = str_locate(txt,"[{;=<> ]")[,1]
  param.name = str_sub(txt,1,end.name-1)
  
  # Find sets for each parameter
  param.sets = extract.sets(txt)
  names(param.sets)=param.name
  
  # Extract those parameters that are defined to be equal to some value
  comment.start = str_locate(txt,fixed("#"))[,1]
  comment.start[is.na(comment.start)] = 1000000  
  param.defined = str_detect(substring(txt,1,comment.start),fixed("="))
  
  
  return(list(sets=sets.name,sets.sets = sets.sets,
              var=var.name,var.sets=var.sets,
              param=param.name,param.sets=param.sets,
              param.defined=param.defined))
}  

#' Generates a GMPL data file
#' 
#' Generates a GMPL data file for the model specified in dat.file
#' sets and param are lists that contain the 
#' values of the sets and parameters that
#' are specified in the GMPL model
#' @export
gmpl.make.dat.file =  function(sets=NULL,param=NULL,mod.file,dat.file=NULL) {
   
  restore.point("gmpl.make.dat.file")
  
  mi = gmpl.get.model.info(mod.file)
  
  mi$param = mi$param[!mi$param.defined]
  mi$param.sets = mi$param.sets[!mi$param.defined]
  
  if (!setequal(names(sets),mi$sets)) {
    print("Error: Model file specifies the following sets:")
    print(mi$sets)
    print("but you specified now the following sets:")
    print(names(sets))
    stop()
  }
  if (!setequal(names(param),mi$param)) {
    print("Error: Model file specifies the following parameters:")
    print(mi$param)
    print("but you specified now the following parameters:")
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
      
      is.grid = FALSE
      #restore.point("test")
      #if (pn=="q")
      #  stop()
      
      if (is.data.frame(p))
        p = as.matrix(p)
   
      if (is.na(mi$param.sets[[pn]])[1]) {
         if (length(p)>1) {
          stop(paste("Error: Model file specifies parameter",pn," as a single number, but you provided a vector: ",pn, "=",paste(p,collapse=" ")))
        }
      } else if (length(pset)==1) {
        if (length(p) != length(pset[[1]])) {
          stop(paste("Error: Parameter ",pn," is defined over set ", paste(names(pset),collapse=" "), " and should have ", length(pset[[1]]), " elements, but the argument has ", length(p), "elements"))
        }
      } else if (length(pset)==2) {
        if (!(is.matrix(p) | is.data.frame(p))) {
          stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you did not provide a matrix."))          
        }
        if (NCOL(p)==3 & NROW(p) == length(pset[[1]]) * length(pset[[2]])) {
          is.grid=TRUE
        } else {  
          if (NROW(p)!=length(pset[[1]]) | NCOL(p)!=length(pset[[2]])) {
            stop(paste("Error: Parameter ",pn," is defined over sets ", paste(names(pset),collapse=" "), " and should be a ",length(pset[[1]]),"x",length(pset[[2]]), " matrix, but you provided a ",NROW(p), "x", NCOL(p), " matrix."))          
          }
        }
      } else if (length(pset)>=3) {
        if (NCOL(p)!=length(pset)+1) {
          stop(paste("Error: Parameter ",pn," is defined over the sets: ", paste(names(pset),collapse=","), ". You must provide a grid with ", length(pset)," index columns and one value column."))
        }
        is.grid = TRUE
      }

      # A matrix
      if (is.matrix(p) & (!is.grid)) {
        tstr = apply(p,1,function (row) paste(row,collapse=" "))
        rowset = pset[[1]]
        colset = pset[[2]]
        tstr = paste("      ",rowset,"  ",tstr,collapse="\n",sep="")
        tstr = mypaste(" param ",pn,": ",paste(colset,collapse=" "),":= \n",
                                         tstr, ";\n")
        
        str[i] = tstr  
      # A parameter defined over a single set
      } else if (is.grid) {
        id.grid = p[,-NCOL(p)]
        id.txt = paste.matrix.cols(id.grid,sep=",")
        row.txt = paste("[",id.txt,"]",p[,NCOL(p)])
        
        str[i] = mypaste("  param ",pn, " := ", paste(row.txt,collapse=" "),";\n")        
        
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
  } else {
    return(txt)
  }
}


gmpl.load.problem = function(mod.file,dat.file) {
  require(glpk)
  lp <- lpx_read_model(mod.file,dat.file)
  lp
}

gmpl.solve = function(mod.file=NULL,dat.file=NULL,lp=NULL,
                      delete.lp = is.null(lp),adapt.sol=ADAPT.SOL) {
  # Solves a GMPL model with a given dat.file
  # and model.file
  
  if (is.null(lp)) {
    print("Load problem...")
    lp = gmpl.load.problem(mod.file,dat.file)
  }
  print("Solve problem...")

  res = glpk.solve(lp,delete.lp)
  
  if (adapt.sol) {
    res$sol = gmpl.adapt.sol(res$sol,mod.file=mod.file)
  }
  return(res)
}
  
glpk.solve = function(lp=NULL, delete.lp = TRUE) {
  # solve model with simplex algorithm
  require(glpk)
  
  
  print("Solve model...")
  code=lpx_simplex(lp)
  print("Retrieve solution (can be slow...)")
  # Retrieve Solution
  nc = lpx_get_num_cols(lp)
  nr = lpx_get_num_rows(lp)
  
  val = lpx_get_obj_val(lp)
  
  sol           = mapply(lpx_get_col_prim,j=1:nc,MoreArgs=list(lp=lp))
  sol.name      = mapply(lpx_get_col_name,j=1:nc,MoreArgs=list(lp=lp))
  names(sol)    = sol.name
  
  shadow.prices = mapply(lpx_get_row_dual,i=1:nr,MoreArgs=list(lp=lp))
  constr.name   = mapply(lpx_get_row_name,i=1:nr,MoreArgs=list(lp=lp))
  names(shadow.prices) = constr.name
  
  # Delete the linear program in order to free memory
  if (delete.lp) {
    lpx_delete_prob(lp)
  }
  
  return(list(code=code,val=val,sol=sol,shadow.prices=shadow.prices))
}

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
    if (NROW(df>1) & NCOL(df)>1) {
      for (i in 1:NCOL(df)) {
        if (is.character(df[1,i]) & !is.na(suppressWarnings(as.numeric(df[1,i]))) ) {
          num.col= suppressWarnings(as.numeric(df[,i]))
          if (sum(is.na(num.col))==0)
            df[,i] = num.col
        }        
      }
    }
    ret[[vn]]=df
  }
  return(ret)
} 

# Converts
matrix.to.grid = function(mat,col.id.name="col.id", row.id.name="row.id",val.name="val",row.grid = NULL) 
{
  restore.point("matrix.to.grid")
  
  long = as.vector(as.matrix(mat))
  row.id = rep(1:NROW(mat), times=NCOL(mat))
  col.id = rep(1:NCOL(mat), each=NROW(mat))
  
  if (is.null(row.grid)) {
    grid = cbind(row.id,col.id,long)
    colnames(grid) = c(row.id.name,col.id.name,val.name)
  } else {
    row.grid = as.matrix(row.grid)
    grid = cbind(row.grid[row.id,],col.id,long)
    colnames(grid) = c(colnames(row.grid),col.id.name,val.name)
  }
  return(grid)  
}
