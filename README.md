sktools
=======

R tools for my courses

To install in R, first install the package devtools from CRAN and then run

library(devtools); install_github(repo="sktools", username="skranz")

To run AMPL files via the NEOS Server, you also need to run

install.packages("XMLRPC", repos = "http://www.omegahat.org/R", type = "source")
install.packages("rneos")
