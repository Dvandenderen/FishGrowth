
##### overview plot
  library(latex2exp)
  
#### plot all fish growth 
####################################################
  
  setwd("..../R-code statistics")
  source("Main_growth_analysis.R")
  
  ### write output in folder:
  setwd("..../Output")
  pdf("Overview_growth.pdf",width=4,height=3.56)   # save 4.00  x 3.56 portrait
  
# sharks and rays
  newx <- seq(min(shray$Temperature-0.1,na.rm=T),max(shray$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(shraymodel)[1] + coef(shraymodel)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  plot(y_out~newxCelsius ,lty =1,type="l", xlim=c(-2,30), ylim=c(1.2,4.4),col="grey",lwd=3,xaxt="n",yaxt="n",
       xlab=TeX("$Temperature$ ^{o}$C"),ylab=paste("Juvenile growth (K . L","\U221E",")"))
  
# large pelagics
  newx <- seq(min(lpel$Temperature-0.1,na.rm=T),max(lpel$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius , col="red", lty=1,lwd=3)

# small pelagics
  newx <- seq(min(spel$Temperature-0.1,na.rm=T),max(spel$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(spelmod)[1] + coef(spelmod)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="red", lty=1,lwd=1.5)  
 
# large demersal
  newx <- seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(ldemmod)[1] + coef(ldemmod)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="blue", lwd=3)  
  
# small demersal
  newx <- seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 <- rep(mean(sdem$Bentbio),length(newx))
  y_out<- coef(sdemmod)[1] + coef(sdemmod)[2]*newx + coef(sdemmod)[3]*newx2
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="blue", lwd=1.5)  

  legend(-2, 4.6, legend=c("pelagic", "demersal", "shark/ray"),
         col=c("red","blue","grey"), lty=c(1,1,1), cex=0.8,
         box.lty=0)
  axis(1,c(0,10,20,30))
  axis(2,c(log(5),log(10),log(20),log(40),log(80)),labels=c(5,10,20,40,80),las=1)

  dev.off()

#### plot all fish asymptotic size 
####################################################
  setwd("..../R-code statistics")
  source("Main_Linf_analysis.R")
  
  ### write output in folder:
  setwd("..../Output")
  pdf("Overview_Linf.pdf",width=4,height=3.56)   # save 4.00  x 3.56 portrait
  
# sharks and rays
  newx <- seq(min(shray$Temperature-0.1,na.rm=T),max(shray$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(shraymodel)[1] + coef(shraymodel)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  plot(y_out~newxCelsius ,lty =1,type="l", xlim=c(-2,30),ylim=c(1.6,6.2), col="grey",lwd=3,xaxt="n",yaxt="n",
       xlab=TeX("$Temperature$ ^{o}$C"),ylab="Asymptotic size (cm)")
  
# large pelagics
  newx <- seq(min(lpel$Temperature-0.1,na.rm=T),max(lpel$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius , col="red", lty=1,lwd=3)
  
# small pelagics
  newx <- seq(min(spel$Temperature-0.1,na.rm=T),max(spel$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 <- rep(mean(spel$Zoobio),length(newx))
  y_out<- coef(spelmodtemp)[1] + coef(spelmodtemp)[2]*newx + coef(spelmodtemp)[3]*newx2
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="red", lty=1,lwd=1.5)  
  
# large demersal
  newx <- seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  y_out<- coef(ldemmod)[1] + coef(ldemmod)[2]*newx
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="blue", lwd=3)  
  
# small demersal
  newx <- seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 <- rep(mean(sdem$Bentbio),length(newx))
  y_out<- coef(sdemmodtemp)[1] + coef(sdemmodtemp)[2]*newx + coef(sdemmodtemp)[3]*newx2
  newxCelsius <- (1/newx)/(8.62*10^-5)-273.2
  lines((y_out)~newxCelsius, col="blue", lwd=1.5)  
  
 # legend(-2, 4.6, legend=c("pelagic", "demersal", "shark/ray"),
 #        col=c("red","blue","grey"), lty=c(1,1,1), cex=0.8,
 #        box.lty=0)
  axis(1,c(0,10,20,30))
  axis(2,c(log(5),log(20),log(80),log(320)),labels=c(5,20,80,320),las=1)
  
  dev.off()
  
