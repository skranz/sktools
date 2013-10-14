#' Uses Manipulate to interactively change some parameters of an image.plot
#'@export
explore.image = function(x,y,z,xlab="x",ylab="y",main="",num.colors=30,add.plot.fun = NULL, pal.colors=c("red","white","blue")) {
  library(fields)
  library(manipulate)
  
  # Specify a palette
  my.palette <- colorRampPalette(pal.colors,space = "Lab")
  library(fields)    
  
  image.fun = function(x,y,z,num.colors,focus,xL,xH,yL,yH,...) {  
    col = my.palette(num.colors)
    zlim = range(z)
    at.rel = seq(0,1,length=NROW(col)+1)
    at.rel = at.rel ^ (abs(focus)^sign(-focus))
    at = zlim[1] + diff(zlim)*at.rel
    
    image.plot(x=x,y=y,z=z,
               main=main,xlab = xlab,ylab = ylab,
               xlim=c(xL,xH),ylim=c(yL,yH),
               col=col,breaks=at) 
    if (!is.null(add.plot.fun)) {
      add.plot.fun(xL=xL,xH=xH,
                   yL=yL,yH=yH,num.colors=num.colors,...)
    }
  }
  
  xrange=range(x)
  yrange=range(y)
  zrange=range(z)
  control = list(
    focus = slider(-100,100,0,step=1,label="focus"),
    num.colors = slider(2,200,30,step=1,label="Number of Colors"),
    xL = slider(xrange[1],xrange[2],xrange[1],step=(xrange[2]-xrange[1])/1000),
    xH = slider(xrange[1],xrange[2],xrange[2],step=(xrange[2]-xrange[1])/1000),             
    yL = slider(yrange[1],yrange[2],yrange[1],step=(yrange[2]-yrange[1])/1000),
    yH = slider(yrange[1],yrange[2],yrange[2],step=(yrange[2]-yrange[1])/1000)
  )
  manipulate(image.fun(x=x,y=y,z=z,num.colors=num.colors,
                       focus=focus,xL=xL,xH=xH,yL=yL,yH=yH),
             control)
}     

#' Uses Manipulate to explore the function z.fun
#' @export
explore.3d.fun = function(z.fun,plot.type="image",xrange,yrange=xrange,main="Function Explorer",xlab="x",ylab="y",num.colors=30,
                          pal.colors=c("red","white","blue"),Vectorize.z.fun = TRUE,grid.length.default=8,num.color.default=100,image.fun=NULL,
                          extra.control = list(),add.plot.fun = NULL,...) {
  
  library(fields)
  library(manipulate)
  # Specify a palette
  my.palette <- colorRampPalette(pal.colors,space = "Lab")
  if (is.null(image.fun)) {
    library(fields)    
    if (plot.type=="image") {
      image.fun = function(x,y,z,col,focus,...) {        
        zlim = range(z)
        at.rel = seq(0,1,length=NROW(col)+1)
        at.rel = at.rel ^ (abs(focus)^sign(-focus))
        at = zlim[1] + diff(zlim)*at.rel
        
        image.plot(x=x,y=y,z=z,
                   main=main,xlab = xlab,ylab = ylab,
                   col=col,breaks=at) 
      }
    } else if (plot.type=="persp") {
      image.fun = function(x,y,z,col,theta=30,phi=20,...) {
        drape.plot(x=x,y=y,z=z,
                   main=main,xlab = xlab,ylab = ylab,
                   col=col,theta=theta,phi=phi) 
      }   
    }
  }
  if (Vectorize.z.fun) {
    z.fun = Vectorize(z.fun, vectorize.args=c("x","y"))
  }   
  f = function(grid.length,xL,xH,yL,yH,num.color,focus,...) {
    n = grid.length
    if (xL>=xH || yL>=yH) return(NULL)
    x = seq(xL,xH,length=n)
    y = seq(yL,yH,length=n)
    xy = expand.grid(x,y)
    z = matrix(z.fun(xy[,1],xy[,2],...),n,n)
    image.fun(x=x,y=y,z=z,col = my.palette(num.color),main=main,xlab=xlab,ylab=ylab,focus=focus,...)
    if (!is.null(add.plot.fun)) {
      add.plot.fun(grid.length=grid.length,xL=xL,xH=xH,yL=yL,yH=yH,num.color,...)
    }
  }
  
  control = list(
    xL = slider(xrange[1],xrange[2],xrange[1],step=(xrange[2]-xrange[1])/1000),
    xH = slider(xrange[1],xrange[2],xrange[2],step=(xrange[2]-xrange[1])/1000),             
    yL = slider(yrange[1],yrange[2],yrange[1],step=(yrange[2]-yrange[1])/1000),
    yH = slider(yrange[1],yrange[2],yrange[2],step=(yrange[2]-yrange[1])/1000),
    grid.length = slider(2,200,grid.length.default,step=1),
    num.color = slider(2,200,num.color.default,step=1)
  )
  if (plot.type=="persp") {
    control = c(list(theta=slider(0,360,30,step=1),
                     phi=slider(0,360,20,step=1)),
                control)
  } else if (plot.type=="image") {
    control = c(list(focus = slider(-100,100,0,step=1,label="focus")),
                control)
  }
  control = c(extra.control,control)
  
  expr = paste(names(control),"=",names(control),collapse=",")
  expr = paste("f(",expr,",...)")
  #print(expr)
  expr = parse(text=expr)[[1]]
  manipulate(eval(expr),control)             
}

examples.explore.3d.fun = function() {
  z.fun = function(x,y,a=1,b=1,c=1,d=1,e=1,...) {
    a*x^2+b*y^2+c*x^3+d*y^3+e*x*y  
  } 
  explore.3d.fun(z.fun=z.fun,plot.type="image",xrange=c(-2,2),Vectorize.z.fun=F,
                 extra.control = list(a=slider(-5.0,5.0,1,step=0.01)),
                 num.color.default=30)
}
