
add.examples2package = function(package,package.path=".", example.path = paste(package.path,"/examples")) {
  objs = ls(paste("package:",package,sep=""))
  
  for (fun.name in objs) {
    example.fun = paste("examples.",fun.name,sep="")
    if (exists(example.fun)) {
      message(paste(fun.name," "))
      add.example2Rd(fun.name,example.fun=example.fun,package.path=package.path)  
    }
  }
}

add.tests2package = function(package,package.path=".", example.path = paste(package.path,"/examples")) {
  objs = ls(paste("package:",package,sep=""))
  
  for (fun.name in objs) {
    example.fun = paste("examples.",fun.name,sep="")
    if (exists(example.fun)) {
      message(paste(fun.name," "))
      add.example2Rd(fun.name,example.fun=example.fun,package.path=package.path)  
    }
  }
}


add.example2Rd = function(fun.name,example=NULL,example.fun=paste("examples.",fun.name,sep=""),package.path=".") {  
  if (is.null(example)) {
    if (!exists(example.fun)) {
      warning(paste("No example code given and no example.fun ", example.fun))
      return()
    }
    
    example = capture.output(print(eval(parse(text=example.fun))))
    if (length(example)>=1)
      example = example[-1]
    if (substring(example[length(example)],1,nchar("<environment:"))=="<environment:") {
      example = example[-length(example)]
    }
    # Remove final }
    example = example[-length(example)]
  }
  example
  
  Rd.file = paste(package.path,"/man/",fun.name,".Rd",sep="")
  if (!file.exists(Rd.file)) {
    warning(paste("No Rd file", Rd.file, " found!"))
    return()
  }
  txt = readLines(Rd.file)
  new.txt = c("\\examples{",example,"}")
  txt = c(txt,new.txt)
  txt
  writeLines(txt,Rd.file)
  invisible(txt)
}

examples.add.example2Rd = function() {
  txt = capture.output(print(eval(parse(text="examples.f"))))
  ret = add.example2Rd(fun.name="quick.by",package.path="D:/libraries/sktools/sktools")
  
  message(ret)
}

g = function() {
  x = 5 +
    10
  30-x == 10
}

# Checks whether any pattern matches in x
any.grepl = function(patterns,x,...) {
  ret = lapply(patterns,grepl,x=x,...)
  ret = do.call(rbind,ret)
  colSums(ret)>0
}

examples.any.grepl = function() {
  any.grepl(c("hallo","hi"), c("hi wie gehts","Na du","hallo"))
}

example2test = function(fun.name=NULL, example.fun=paste("examples.",fun.name,sep=""), max.nchar = 500, verbose = TRUE, ignore.calls = c("benchmark")) {
  restore.point("example2test")
  
  ex = eval(parse(text=paste("body(",example.fun,")")))
  com.str = as.character(ex)[-1]
  com.str = c("set.seed(1000)",com.str)

  # Identify lines of code in which the to be tested function is called
  fun.regexp = paste("(\\Q", fun.name, "\\E)([[:space:]]*)\\(",sep="")
  calls.fun = any.grepl(fun.regexp,com.str)
  
  ignore.calls = paste("(\\Q", ignore.calls, "\\E)([[:space:]]*)\\(",sep="")
  ignore.lines = any.grepl(ignore.calls, com.str)
  
  calls.fun = calls.fun & (!ignore.lines)
  
  #com.str[calls.fun]
  
  
  last.row = max(which(calls.fun))
  if (length(last.row)>0) {
    com.str = com.str[1:last.row]
  }
  test.str = rep("", length(com.str))
  
  work.env = emptyenv()
  for (i in seq_along(com.str)) {
    err = try(val <- eval(parse(text=com.str[i]),envir=work.env), silent=TRUE)
    if (is(err,"try-error")) {
      if (calls.fun[i])
        test.str[i] = paste("\n  expect_error(eval(expression(", com.str[i], ")))")
      com.str[i] = paste("#", com.str[i])      
    } else {
      
      if (calls.fun[i]) {
        val.str = paste(deparse(val, width.cutoff=500L),collapse="\n")
        if (nchar(val.str)<max.nchar) {
          test.str[i] = paste("\n  expect_equivalent(\n  eval(expression(", com.str[i], ")),\n  ", val.str,"\n)\n")
        } else {
          require(digest)
          test.str[i] = paste("\n  expect_equivalent(\n  digest(eval(expression(", com.str[i], "))),\n  ",'"', digest(val),'"',"\n)\n",sep="")
          #test.str[i] ="# Test ommited result too long"
        }
        com.str[i] = paste("#", com.str[i])
      }
    }
    if (verbose)
     message(paste("\n  ",com.str[i],test.str[i],sep=""))
  }
  code = paste("\n  ",com.str, test.str, collapse="",sep="")
  
  code = paste('test_that("test.',example.fun,'",{\n',code,'\n})',sep="")
  #capture.output(cat(code))
  code
}
