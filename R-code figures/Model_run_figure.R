library(latex2exp)

#### Using a standard bioenergetics model, we show that a 
   # lower thermal sensitivity of feeding rates (relative to metabolism)
   # can result in a range of growth responses similar to our observations
####################################################
  setwd("..../Output")
  
  pdf("Model_figure.pdf",width=4,height=3.56)  # save 4.00 x 3.6 portrait
  par(mar=c(2, 4, 2, 1))
# define Q10 scaling of feeding rates
  sc<-c(2.5,2.2,1.9,1.8)

# define line colors plot
  colli<-c("black","red","blue","grey")

# create empty plot
  plot(x=1,y=1,col="white",xlim=c(0,30),ylim=c(0,1),yaxt="n",xaxt="n",xlab=TeX("Temperature$ ^{o}$C"),
       ylab="Modeled growth (A)",main="")
  axis(1,c(0,15,30))
  axis(2,c(0,0.3,0.6,1),labels=c(1,2,4,10),las=1)

# run the model for different temperature scalings of the feeding rates (while keeping a maintenance Q10 of 2.5)
  for (i in 1:4){
    alpha <-0.6
    B <- 2
    Temp <- seq(0,30,1)
    
    Q10 <- 2.5
    TQ10<- Q10^((Temp-15)/10)
    k <- 6*TQ10
    
    mQ10 <- sc[i]
    mTQ10 <- mQ10^((Temp-15)/10)
    gamma <- 20*mTQ10
    Cmax <- 30*mTQ10
    
    A  <- alpha*gamma*B/(gamma*B+Cmax)*Cmax-k
    feedinglevel <- gamma*B/(gamma*B+Cmax)
    #Q10_A <- A[21]/A[11]
    #print(Q10_A)
    lines(log10(A)~Temp,type="l",lwd=2,col=colli[i],lty=i)
  }
  legend(x=12,y=0.5,lwd=2,sc,col=colli,lty=c(1:4),title=TeX("$Q_{10}$"),bty = "n")
  
  dev.off()
