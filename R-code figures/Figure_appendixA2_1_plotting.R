# illustration of growth coefficient A with von Bertalanffy curves
###################################################################
  A <- c(5,5,10,10) # growth coefficient A
  f <- c(0.5,2,0.5,2) # loss coefficient f
  n <- 2/3 # setting the exponent
  idx1 <- c(1,3,5,7) # setting index1 for loop
  idx2 <- c(2,4,6,8) # setting index2 for loop
  #(A/f)^3 # maximum weight

# set an empty data matrix
  w <- matrix(NA,nrow=21,ncol=8) # matrix with empty data
  w[1,c(1,3,5,7)] <- 0.01        # weight at age
  w[1,c(2,4,6,8)] <- NA          # specific weight at age

# calculate the weight and specific weight over time
  for (j in 1:4){
    for (i in 1:20){
    change <- A[j] * w[i,idx1[j]]^n - f[j] * w[i,idx1[j]]
    w[i+1,idx1[j]] <- change + w[i,idx1[j]]
    w[i,idx2[j]] <-  change
    }}
  
  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  pdf("Growth_illustration.pdf",width=6.9,height=3.56)
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
  
  l1 <- (w[,1]/0.01)^(1/3)
  plot(l1,type="l",lwd=2,ylim=c(1,100),ylab="length (cm)",xlab= "Time (years)", yaxt="n", xaxt="n",col="grey")
  l3 <- (w[,3]/0.01)^(1/3)
  lines(l3,col="#ef3b2c",lwd=2)
  l5 <- (w[,5]/0.01)^(1/3)
  lines(l5,col="grey",lwd=2,lty=2)
  l7 <- (w[,7]/0.01)^(1/3)
  lines(l7,col="#ef3b2c",lwd=2,lty=2)
  axis(1, c(1,11,21),c("0","10","20"))
  axis(2,c(0,50,100),las=1)
  
  library(latex2exp)
  plot((w[,2]/w[,1]), xlim=c(1,9), ylim=c(0,50), type ="l", xlab="Time (years)",
       ylab=TeX("Specific growth (dw/dt)/w"),xaxt="n",yaxt="n",lwd=2,col="grey")
  
  axis(1,c(1,5,9),c("0","4","8"))
  axis(2,c(0,25,50),las=1)
  lines((w[,4]/w[,3]), col="#ef3b2c",lwd=2)
  lines((w[,6]/w[,5]), col="grey",lty=2,lwd=2)
  lines((w[,8]/w[,7]), col="#ef3b2c",lty=2,lwd=2)
  
  legend(4.5,50, legend=c("A = 5, f = 0.5", "A = 5, f = 2", "A = 10, f = 0.5","A = 10, f = 2"),
         col=c("grey","#ef3b2c","grey","#ef3b2c"), lty=c(1,1,2,2),lwd=c(2,2,2,2), cex=0.8,
         box.lty=0, y.intersp=1)
  
  dev.off()


