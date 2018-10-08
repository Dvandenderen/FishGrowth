
##### growth data plots
  library(latex2exp)
  
  setwd("..../R-code statistics")
  source("Main_Linf_analysis.R")

#### plot all fish growth 
####################################################
  setwd("..../Output")
  
  pdf("Linf_allfish.pdf",width=4,height=3.56)   # save 4.00  x 3.56 portrait
  par(mar=c(2, 4, 2, 1))
  plot(log(fishes$Linf)~fishes$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="1/cT",ylab="ln(asymptotic size, cm)",main="All fishes")
  
  # get the model
  Allmod <- lm(log(Linf)~Temperature, data = fishes)
  newx = seq(min(fishes$Temperature-0.1,na.rm=T),max(fishes$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(Allmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(Allmod)[1] + coef(Allmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2,lty=3)
  
  # get the Q10 for all fish
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Allmod)[1] + coef(Allmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_All<-y20/y10 
  
  axis(1,c(39,40,41,42))
  axis(2,c(2.3,4.6,6.9),las=1)
  text(42, 6.5, TeX("$Q_{10}$ of 0.95"), cex=1)

  dev.off()
#### plot growth per functional group 
####################################################
  pdf("Linf_guilds.pdf",width=6,height=3.56)  # save 6.00 x 3.56 portrait
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))

# large pelagics
  plot(log(lpel$Linf) ~ lpel$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="",ylab="ln(asymptotic size, cm)",main="Large pelagics")
  
  newx = seq(min(lpel$Temperature-0.1,na.rm=T),max(lpel$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(Lpelmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(2.3,4.6,6.9),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 0.82"), cex=1)

# small pelagics
  plot(log(spel$Linf) ~ spel$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="",ylab="",main="Small pelagics")
  
  newx = seq(min(spel$Temperature-0.1,na.rm=T),max(spel$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 = rep(mean(spel$Zoobio),length(newx))
  conf_interval <- predict(spelmodtemp, newdata=data.frame(Temperature=newx, Zoobio = newx2), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(spelmodtemp)[1] + coef(spelmodtemp)[2]*newx + coef(spelmodtemp)[3]*newx2
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(2.3,4.6,6.9),labels=c("","",""),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 0.96"), cex=1)

# shark and ray
  plot(log(shray$Linf)~shray$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="",ylab="",main="Sharks and rays")
  
  newx = seq(min(shray$Temperature-0.1,na.rm=T),max(shray$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(shraymodel, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(shraymodel)[1] + coef(shraymodel)[2]*newx
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(2.3,4.6,6.9),labels=c("","",""),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 0.91"), cex=1)

# large demersals
  plot(log(ldem$Linf) ~ ldem$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="1/cT",ylab="ln(asymptotic size, cm)",main="Large demersal")
  
  newx = seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(ldemmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(ldemmod)[1] + coef(ldemmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42))
  axis(2,c(2.3,4.6,6.9),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 0.96"), cex=1)

# small demersals 
  plot(log(sdem$Linf) ~ sdem$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="1/cT",ylab="",main="Small demersals")
  
  newx = seq(min(sdem$Temperature-0.1,na.rm=T),max(sdem$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 = rep(mean(sdem$Bentbio),length(newx))
  conf_interval <- predict(sdemmodtemp, newdata=data.frame(Temperature=newx, Bentbio = newx2), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(sdemmodtemp)[1] + coef(sdemmodtemp)[2]*newx +coef(sdemmodtemp)[3]*newx2
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42))
  axis(2,c(2.3,4.6,6.9),labels=c("","",""),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 1.08"), cex=1)

### deep sea fish
  plot(log(deep$Linf)~deep$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(0.9,6.9),
       xlab="1/cT",ylab="",main="Deep living")
  
  newx = seq(min(deep$Temperature-0.1,na.rm=T),max(deep$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(deepmodel, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(deepmodel)[1] + coef(deepmodel)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(2.3,4.6,6.9),labels=c("","",""),las=1)
  text(41.5, 6.7, TeX("$Q_{10}$ = 0.57"), cex=1)
  
  dev.off()
  