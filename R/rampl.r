#source("D:/libraries/rampl/rgmpl.r")


# Notes:
# 1. Don't add a line "end;" in a model or data file
# 2. Don't load model or data within the run file


AMPL.LOCAL.OPTIONS = list(
  "PATH" = c("option solver pathampl;","option presolve 0;")
)

AMPL.NEOS.OPTIONS = list(
  "PATH" = c("option presolve 0;")
  # KNITRO OPTIONS FROM EXAMPLE OF CHE-LIN SU                         
  ,"KNITRO" = c("option knitro_options 'alg=1 outlev=3 opttol=1.0e-6 feastol=1.0e-6'")  
)

ampl.make.dat.file =  function(...) {gmpl.make.dat.file(...)}

neos.ampl.solvers = function() {
  library(rneos)  
  txt = NlistAllSolvers(convert = TRUE, nc = CreateNeosComm())@ans
  dat = as.data.frame(fast_str_split_fixed(":",txt,3,fixed=TRUE))
  colnames(dat) = c("category","solver","inputMethod")
  dat = dat[dat[,"inputMethod"]=="AMPL" & dat[,1] != "kestrel",]
  ord = order(dat[,"solver"])
  cat = fast_str_split_fixed(":",NlistCategories()@ans,2,fixed=TRUE)
  
  cat.row = match(dat[,1],cat[,1])
  dat$descr = cat[cat.row,2]
  dat = dat[ord,]
  rownames(dat) = NULL
  dat
}
examples.neos.ampl.solvers = function() {
  neos.ampl.solvers()
}

ampl.run.neos = function(
  name="",category="cp", solver="PATH",path=getwd(), wait = TRUE,
  model.file=paste(path,"/",name,".mod",sep=""),
  data.file=paste(path,"/",name,".dat",sep=""),
  run.file=paste(path,"/",name,".run",sep=""),
  log.file = paste(path,"/log_",name,"_",solver,".txt",sep=""))
{
  restore.point("ampl.run.neos")
  
  library(rneos)

  display.start.log(log.file,append=FALSE)
  #NlistAllSolvers(convert = TRUE, nc = CreateNeosComm())
  
  ## import of file contents
  modc <- paste(paste(readLines(model.file), collapse = "\n"), "\n")
  datc <- paste(paste(readLines(data.file), collapse = "\n"), "\n")
  runc <- paste(paste(readLines(run.file), collapse = "\n"), "\n")

  #
  template <-NgetSolverTemplate(category = category, solvername = solver,inputMethod = "AMPL")
  ## create list object
  argslist <- list(model = modc, data = datc, commands = runc,comments = "")
  ## create XML string
  xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)

  
  job <- NsubmitJob(xmlstring = xmls, user = "rneos", interface = "",id = 0)
  
  display(NprintQueue(convert = TRUE, nc = CreateNeosComm())@ans)
  #print(NgetJobInfo(obj = job, convert = TRUE))
  
  if (wait) {
    start.len = 1
    while(TRUE) {
      res = NgetIntermediateResults(obj = job, convert = TRUE)@ans
      res = strsplit(res,"\n",fixed=TRUE)[[1]]
      if (length(res)>=start.len) {
        display(res[start.len:length(res)])
      }
      start.len = length(res)+1;
      flush.console()
      Sys.sleep(0.1)
      
      #print(NgetJobInfo(obj = job, convert = TRUE))
      do.stop = NgetJobInfo(obj = job, convert = TRUE)@ans[4] == "Done"
      #print(NgetFinalResultsNonBlocking(job, convert = TRUE))
      if (do.stop) {
        break
      }
    }
    res = NgetIntermediateResults(obj = job, convert = TRUE)@ans
    res = strsplit(res,"\n",fixed=TRUE)[[1]]
    if (length(res)>=start.len) {
      display(res[start.len:length(res)])
    }
    #display(NgetIntermediateResults(obj = job, convert = TRUE)@ans) 
    
    display("Read solution...")
    dat = extract.all.var.from.AMPL.out(res)
    display("Solution has been read...")
    display.stop.log()
    
    return(dat)
  } else {
    display(NgetIntermediateResults(obj = job, convert = TRUE)@ans)
    # Print call 
    display("NgetIntermediateResults(obj = job, convert = TRUE)")
  }
  display.stop.log()
  return (job)
}

ampl.make.run.file = function(name,run.name=name,options="",
                         var.out = NULL,neos=FALSE,path=getwd(), 
                         mod.file=paste(path,"/",name,".mod",sep=""),
                         run.file=paste(path,"/",run.name,".run",sep=""),
                         dat.file=paste(path,"/",run.name,".dat",sep="")) {
  
 str = paste(options,"\n\n",collapse="\n")
 if (!neos) {
    str = paste(str,
      "model ", mod.file,";\n",
      "data ",dat.file,";\n\n"
      ,sep="")
  }
  str = paste(str,"solve;\n\n")
  
  mi = gmpl.get.model.info(mod.file)
  if (is.null(var.out)) {
    var.out = mi$var
  }
  
  
  var.sets.str= sapply(mi$var.sets[var.out], function(sets) {
    if (is.na(sets[1]))
      return('""')
    paste('"',sets,'"',collapse=",",sep="")
  })
  var.str = paste(
    'print "#!STARTOUT:', var.out, '";\n',
    'print ', var.sets.str,';\n',
    "display ",var.out,";\n",
    'print "#!ENDOUT:', var.out, '";\n',
    collapse="\n",sep="")
  str = paste(str,
"
option display_1col 10000000;
option display_width 1000;
",              
              var.str)
  
  writeLines(str,run.file)
}  

extract.all.var.from.AMPL.out = function(str) {
  restore.point("extract.all.var.from.AMPL.out")
  
  start.ind = which(str_sub(str,start=0,end=11)=="#!STARTOUT:")
  end.ind   = which(str_sub(str,start=0,end=9)=="#!ENDOUT:")
  var.names = str_sub(str[start.ind],start=12)

  if (length(var.names)>0) {
    ret=lapply(1:length(var.names),
               function(i) {
                 txt = str[(start.ind[i]+1):(end.ind[i]-1)]
                 extract.var.from.AMPL.out(txt)
               })
    names(ret)=var.names
  } else {
    ret = list()
  }
  return(ret)
}


extract.var.from.AMPL.out = function(str) {
  restore.point("extract.var.from.AMPL.out")
  sets = str_trim(str[1])
  str = str[-1]
  is.scalar = sets == ""
  
  if (is.scalar) {
    val = as.numeric(str_trim(str_split_fixed(str[1],"=",n=2)[1,2]))
  } else {
    # replace spaces by just one space
    str = str_trim(str_replace_all(str,"( )+"," "))  
    end = grep(";",str,fixed=TRUE)-1
    str = str[2:end]
    ncol = NROW(str_locate_all(str[1],fixed(" "))[[1]])+1
    mat = fast_str_split_fixed(" ",str,ncol=ncol,fixed=TRUE)    
    #print(ncol)
    if (ncol==2) {
      val = as.numeric(mat[,ncol])
      names(val) = mat[,1] 
    } else if (ncol==3) {
      restore.point("my.restore")
      rn = unique(mat[,1])
      cn = unique(mat[,2])
      nr = length(rn)
      nc = length(cn)
      val = matrix(as.numeric(mat[,ncol]),nr,nc,byrow=TRUE)
      rownames(val) = rn
      colnames(val) = cn
    } else {
      val = as.data.frame(mat)
      val[,ncol] = as.numeric(val[,ncol])
      colnames(val) = c(str_split(sets, fixed(" ")),"val")
    }
  }
  return(val)
}



ampl.run.local = function(name="",path=getwd(),                        run.file=paste(path,"/",name,".run",sep=""), display=TRUE) {
  command = paste("ampl",' "',run.file,'"',sep="") 
  
	ret = system(command,intern = TRUE,wait=TRUE,ignore.stdout = FALSE, ignore.stderr = FALSE, show.output.on.console=TRUE, invisible=FALSE)
  
  if (display)
    display(ret)
  extract.all.var.from.AMPL.out(ret)
}

.DISPLAY.LOG = NULL
display.start.log = function(log.file,append=FALSE) {
  if (append) {
    .DISPLAY.LOG = file(log.file,open="at")
  } else {
    .DISPLAY.LOG = file(log.file,open="wt")
  }
}
display.stop.log = function() {
  .DISPLAY.LOG = NULL
}

# Display stuff in a convenient form
display = function(...,collapse="\n",sep="") {
  str = paste("\n",paste(...,collapse=collapse,sep=sep),"\n",sep="")
  if (!is.null(.DISPLAY.LOG)) {
    write(str,.DISPLAY.LOG,append=TRUE)
  }
  invisible(message(str))
  #print(str,quote=FALSE)
}

fast_str_split_fixed = function(pattern,text,ncol=NULL,...) {
  restore.point("fast_str_split_fixed")
  if (NROW(text)==0) {
    return(NULL)
  }
  if (is.null(ncol)) {
    ncol = length(gregexpr(pattern,text[1],...)[1])+1
  }
  if (ncol==1)
    return(text)
  library(data.table)
  mat = as.data.table(matrix(NA,length(text),ncol))
  for (i in 1:(ncol-1)) {
    #right.pos  = regexpr(pattern,text,...)
    right.pos  = regexpr(pattern,text,fixed=TRUE)
    mat[,i:=substring(text,1,right.pos-1), with=FALSE]
    text = substring(text,right.pos + attr(right.pos,"match.length"))
  }
  mat[,ncol:=text, with=FALSE]
  return(as.matrix(mat))
}
