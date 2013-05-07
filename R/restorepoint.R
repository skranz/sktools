# Debug with the R console by setting restore points.


.onLoad <- function(libname, pkgname) {
  init.restore.point()  
}

init.restore.point = function() {
  rpglob$options = list(storing=TRUE,use.browser = FALSE)
  rpglob$OBJECTS.LIST <- list()
}
rpglob <- new.env()

#' Set and retrieve global options for restore points
#' 
#' @param options a list of options that shall be set. Possible options are listed below
#' @param storing TRUE / FALSE enable or disable storing of options, setting storing = FALSE basicially turns off debugging via restore points
#' @param use.browser TRUE/FALSE If FALSE (default), when options are restored they are simply copied into the global environment and the R console is directly used for debugging. If TRUE a browser mode will be started when objects are restored. It is still possible to parse all R commands into the browser and to use copy and paste. To quit the browser press ESC in the R console. The advantage of the browser is that all objects are stored in a newly generated environment that mimics the environemnt of the original function, i.e. global varariables are not overwritten. Furthermore in the browser mode, one can pass the ... object to other functions, while this does not work in the global environment. The drawback is that at least on my computer the approach does not currently work under RStudio, which crashes when it is attempted to parse a command from the console using the parse() command.
#' @export
restore.point.options = function(options=NULL,...) {
  options = c(options,list(...))
  unknown.options = setdiff(names(options),names(rpglob$options)) 
  if (length(unknown.options)>0) {
    warning(paste("unknown options", paste(unknown.options, collapse=","),"ignored"))
    options = options[setdiff(names(options),unknown.options)]
  }
  if (length(options)>0)
    rpglob$options[options] = options
  rpglob$options
}

#' Retrieves the list of all stored objects
#' @export
get.stored.object.list = function() {
  rpglob$OBJECTS.LIST
}

#' Set whether objects shall be stored or not
#' 
#' @param store if FALSE don't store objects if restore.point or store.objects is called. May save time. If TRUE (default) turn on storage again.
#' @export
set.storing <- function(storing=TRUE) {
  rpglob$options$storing <- storing
}


#' Check whether objects currently are stored or not
#' @export
is.storing <- function() {
  return(rpglob$options$storing)
}




#' Sets a restore point
#'
#' The function behaves different when called from a function or when called from the global environemnt. When called from a function, it makes a backup copy of all local objects and stores them internally under a key specified by name. When called from the global environment, it restores the previously stored objects by copying them into the global environment. See the package Vignette for an illustration of how this function can facilitate debugging.
#'
#' @param name key under which the objects are stored. For restore points at the beginning of a function, I would suggest the name of that function.
#' @param deep.copy if TRUE (default) try to make deep copies of  objects that are by default copied by reference. Works so far for environments (recursivly) and data.tables. The function will search lists whether they contain reference objects, but for reasons of speed not yet in other containers. E.g. if an evironment is stored in a data.frame, only a shallow copy will be made. Setting deep.copy = FALSE may be useful if storing takes very long and variables that are copied by reference are not used or not modified.
#' @param force store even if set.storing(FALSE) has been called
#' @export
restore.point = function(name,deep.copy = TRUE, force=FALSE,
  dots = eval(substitute(list(...), env = parent.frame())),
  use.browser = restore.point.options()$use.browser
) {

  envir = sys.frame(-1);
  
  
  # restore objects if called from the global environment
  # when called from a function store objects
  restore = identical(.GlobalEnv,envir)

  if (!is.null(attributes(envir)$IS.RESTORE.POINT.ENVIRONMENT)) {
    restore = TRUE
  }
  
  if (restore) {
    if (use.browser) {
      restore.point.browser(name,was.forced=force)
    } else {
      restore.objects(name=name,was.forced=force)
    }
  } else {
    store.objects(name=name,parent.num=-2, deep.copy=deep.copy, force=force,dots=dots)
  }
}



#' Stores all local objects of the calling environment to be able to restore them later when debugging
#' 
#' @param name key under which the objects are stored, typical the name of the calling function. If name is NULL by default the name of the calling function is chosen
#' @param deep.copy if TRUE (default) variables that are copied by reference (in the moment environments and data.tables)  will be stored as deep copy. May take long for large variables but ensures that the value of the stored variable do not change
#' @param force store even if do.store(FALSE) has been called
#' @param store.if.called.from.global if the function is called from the global environment and store.if.called.from.global FALSE (default) does not store objects when called from the global environment but does nothing instead.
#' @return returns nothing, just called for side effects
#' @export
store.objects = function(name=NULL,parent.num=-1,deep.copy = TRUE, force=FALSE, store.if.called.from.global = FALSE, envir = sys.frame(parent.num), dots = eval(substitute(list(...), env = parent.frame()))
) {
   
  if (!(is.storing()) & !force) {
    return(NULL)
  }
  
	if (sys.nframe() < 2 & !store.if.called.from.global) {
		warning(paste("store.objects(\"",name,"\") ignored since called from global environment."),sep="")
		return()
	}

  #envir = sys.frame(parent.num);
  
  # Assign name of the calling function
  fun.name = all.names(sys.call(parent.num))[1]
  if (is.null(name)) {
  	name = fun.name
  }
  if (force) {
    warning(paste("store.objects called by ", fun.name, " with force!"))
  }
  
  if (deep.copy) {
    rpglob$copied.ref = NULL
    copied.env = clone.environment(envir,use.copied.ref = TRUE)
  } else {
    copied.env = as.environment(as.list(envir))
  }
  
  ev.dots <- NULL
  try(ev.dots <- force(dots), silent=TRUE)
  dots <- ev.dots
  if (!is.null(dots)) {
    dots = clone.list(dots)
  }
  attr(copied.env,"dots") <- dots
  
  rpglob$OBJECTS.LIST[[name]] <- copied.env
  return()  
}


clone.list = function(li, use.copied.ref = FALSE) {          
  ret.li = lapply(li,copy.object,use.copied.ref = use.copied.ref)
  return(ret.li)
}

clone.environment = function(env, use.copied.ref = FALSE) {
  #print(as.list(env))
  #browser()
  li = eapply(env,copy.object,use.copied.ref = use.copied.ref)
  return(as.environment(li))
}

copy.object = function(obj, use.copied.ref = FALSE) {
  #print("copy.object")
  #print(paste("missing: ",missing(obj), "class(obj) ", class(obj)))
          
  # Dealing with missing values
  if (is.name(obj)) {
    return(obj)
  }
  oclass =class(obj) 
  
  # If the objects has already been copied, just return the reference of the copied version, don't create an additional copy
  
  if (any(oclass %in% c("environment","data.table"))) {
    if (use.copied.ref & !is.null(rpglob$copied.ref)) {
      ind = which(sapply(rpglob$copied.ref[,1],identical,y=obj))
      if (length(ind)>0) {
        return(rpglob$copied.ref[ind[1],2][[1]])
      }
    }
    if ("environment" %in% oclass) {
      copy = clone.environment(obj,use.copied.ref = use.copied.ref)
      
    } else if ("data.table" %in% oclass) {
      copy = data.table::copy(obj)
    }    
    # Store a copy of the reference
    rpglob$copied.ref = rbind(rpglob$copied.ref,c(obj,copy))
  } else {
    if (is.list(obj) & !(is.data.frame(obj))) {
      copy = clone.list(obj,use.copied.ref = use.copied.ref)
    } else {
      copy = obj
    }
  }
  return(copy)
}

# Examing a restore point by invoking the browser
restore.point.browser = function(name,was.forced=FALSE) {
  message(paste("restore point",name))
  
  
  # Parser function: Needs to be stopped by pressing ESC
  parse.fun = function(...) {
    while(TRUE) {
      try({
        expr.out <- capture.output(eval(parse(prompt="RP: ")))
        message(expr.out)
      })
    }
  }
  
  # Set environment of parse.fun
  enclos.env=.GlobalEnv # may store an enclosing environment instead
  env <- environment(parse.fun) <- new.env(parent=enclos.env)
  attr(env,"IS.RESTORE.POINT.ENVIRONMENT") <- TRUE
  
  # Populate environment with stored variables
  restore.objects(name,dest=env,was.forced=was.forced)
  
  # Get ... from original function
  dots = get.stored.dots(name)
  do.call(parse.fun,dots)
  
}

#' Restore stored objects by copying them into the global environment
#' 
#' @param name name under which the variables have been stored
#' @param dest environment into which the stored variables shall be copied. By default the global environment.
#' @param was.forced flag whether storage of objects was forced. If FALSE (default) a warning is shown if restore.objects is called and is.storing()==FALSE, since probably no objects have been stored.
#' @return returns nothing but automatically copies the stored variables into the global environment
#' @export
restore.objects = function(name, dest=globalenv(), was.forced=FALSE) {
  if ((!is.storing()) & (!was.forced)) 
    warning("is.storing() == FALSE\nPossible objects were not correctly stored. Call set.storing(TRUE) to enable storing.")
  
  env =   rpglob$OBJECTS.LIST[[name]]

  
  
  # Clone stored environment in order to guarantee that the restore point can be used several times even if reference objects are used
  rpglob$copied.ref = NULL
  cenv = clone.environment(env,use.copied.ref = TRUE)
  # Copy the stored objects into the enviornment specified by dest (usually the global environment)
  copy.into.env(source=cenv,dest=dest)
  
  #message(paste("Dest Environment: ", paste(ls(envir=dest),collapse=",")))
  message(paste("Restored: ", paste(ls(envir=cenv),collapse=",")))
}

get.stored.dots = function(name) {
  env  = rpglob$OBJECTS.LIST[[name]]
  dots = attributes(env)$dots
  if (length(dots)>0)
    dots = clone.list(dots)
  dots
}

#check.global.vars()

#' Copies all members of a list or environment into an environment
#' 
#' @param source a list or environment from which objects are copied
#' @param the enviroenment into which objects shall be copied
#' @param names optionally a vector of names that shall be copied. If null all objects are copied
#' @param exclude optionally a vector of names that shall not be copied
#' @export
copy.into.env = function(source=sys.frame(sys.parent(1)),dest=sys.frame(sys.parent(1)),names = NULL, exclude=NULL) {

  if (is.null(names)) {
    if (is.environment(source)) {
      names = ls(envir=source)
    } else {
      names = names(source)
    }
  }
  names = setdiff(names,exclude)
  
  if (is.environment(source)) {
    for (na in names) {
      assign(na,get(na,envir=source), envir=dest)
    }
  } else if (is.list(source)) {
    for (na in names) {
      assign(na,source[[na]], envir=dest)
    }
  }
}



