
## plot fish asymptotic length with temperature - Figure S1-2
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)

  plot(fishes$LLinf~fishes$Temperature,las=1,xlab= TeX("Temperature ($^{o}C$)"),ylab=TeX("$log_{10}$(asymptotic length) (cm)"))
  summary(lm(fishes$LLinf~fishes$Temperature))

  x <- c(-2:29)
  y <- 1.6750398 + -0.0026331 * x
  lines(y~x, col="blue",lwd=2)  
