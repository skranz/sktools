\name{explore.3d.fun}
\alias{explore.3d.fun}
\title{Uses Manipulate to explore the function z.fun}
\usage{
  explore.3d.fun(z.fun, plot.type = "image", xrange,
    yrange = xrange, main = "Function Explorer",
    xlab = "x", ylab = "y", num.colors = 30,
    pal.colors = c("red", "white", "blue"),
    Vectorize.z.fun = TRUE, grid.length.default = 8,
    num.color.default = 100, image.fun = NULL,
    extra.control = list(), add.plot.fun = NULL, ...)
}
\description{
  Uses Manipulate to explore the function z.fun
}

\examples{\dontrun{
  z.fun = function(x,y,a=1,b=1,c=1,d=1,e=1,...) {
    a*x^2+b*y^2+c*x^3+d*y^3+e*x*y  
  } 
  explore.3d.fun(z.fun=z.fun,plot.type="image",xrange=c(-2,2),Vectorize.z.fun=F,
                 extra.control = list(a=slider(-5.0,5.0,1,step=0.01)),
                 num.color.default=30)
}}
