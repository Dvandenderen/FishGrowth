
##### growth data plots
  library(latex2exp)
  
  setwd("..../R-code statistics")
  source("Main_growth_analysis.R")

#### plot all fish growth 
####################################################
  setwd("..../Output")
  
  pdf("Growth_allfish.pdf",width=4,height=3.56)   # save 4.00  x 3.56 portrait
  par(mar=c(2, 4, 2, 1))
  plot(log(fishes$Arate)~fishes$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
   xlab="1/cT",ylab=paste("ln(Juvenile growth - K . L","\U221E",")"),main="All fishes")
  
  # get the model
  Allmod <- lm(log(Arate)~Temperature, data = fishes)
  newx = seq(min(fishes$Temperature-0.1,na.rm=T),max(fishes$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(Allmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(Allmod)[1] + coef(Allmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  
  # get the Q10 for all fish
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Allmod)[1] + coef(Allmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_All<-y20/y10 
  
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),las=1)
  text(42, 6.1, TeX("$Q_{10}$ of 1.36"), cex=1)
  dev.off()
 
#### plot growth per functional group 
####################################################
  pdf("Growth_guilds.pdf",width=6,height=3.56)  # save 6.00 x 3.56 portrait
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))

# large pelagics
  plot(log(lpel$Arate) ~ lpel$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="",ylab=paste("Juvenile growth (K . L","\U221E",")"),main="Large pelagics")
  
  newx = seq(min(lpel$Temperature-0.1,na.rm=T),max(lpel$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(Lpelmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(0,2.3,4.6),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.27"), cex=1)

# small pelagics
  plot(log(spel$Arate) ~ spel$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="",ylab="",main="Small pelagics")
  
  newx = seq(min(spel$Temperature-0.1,na.rm=T),max(spel$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(spelmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(spelmod)[1] + coef(spelmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.61"), cex=1)
  
# shark and ray
  plot(log(shray$Arate)~shray$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="",ylab="",main="Sharks and rays")
  
  newx = seq(min(shray$Temperature-0.1,na.rm=T),max(shray$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(shraymodel, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(shraymodel)[1] + coef(shraymodel)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.96"), cex=1)
  
# large demersals
  plot(log(ldem$Arate) ~ ldem$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab=paste("Juvenile growth (K . L","\U221E",")"),main="Large demersal")
  
  newx = seq(min(ldem$Temperature-0.1,na.rm=T),max(ldem$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(ldemmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(ldemmod)[1] + coef(ldemmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.12"), cex=1)
  
# small demersals 
  plot(log(sdem$Arate) ~ sdem$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab="",main="Small demersals")
  
  newx = seq(min(sdem$Temperature-0.1,na.rm=T),max(sdem$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 = rep(mean(sdem$Bentbio),length(newx))
  conf_interval <- predict(sdemmod, newdata=data.frame(Temperature=newx, Bentbio = newx2), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(sdemmod)[1] + coef(sdemmod)[2]*newx +coef(sdemmod)[3]*newx2
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.49"), cex=1)
  
  ### deep sea fish
  plot(log(deep$Arate)~deep$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab="",main="Deep living")
  
  newx = seq(min(deep$Temperature-0.1,na.rm=T),max(deep$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(deepmodel, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(deepmodel)[1] + coef(deepmodel)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 0.66"), cex=1)
  
  dev.off()
  
#### plot growth per sub-functional group small demersal
####################################################
  pdf("Growth_subgroups.pdf",width=6,height=3.56)  # save 6.00 x 3.56 portrait
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))

# reef-associated fish
  plot(log(reef$Arate)~reef$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab="",main="Reef-associated")
  
  newx = seq(min(reef$Temperature-0.1,na.rm=T),max(reef$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(reefmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(reefmod)[1] + coef(reefmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(0,2.3,4.6),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.40"), cex=1)

# benthopelagics 
  plot(log(BenPo$Arate)~BenPo$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab="",main="Benthopelagics")
  
  newx = seq(min(BenPo$Temperature-0.1,na.rm=T),max(BenPo$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 = rep(mean(BenPo$Bentbio),length(newx))
  conf_interval <- predict(benpomod_temp, newdata=data.frame(Temperature=newx,Bentbio=newx2), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(benpomod_temp)[1] + coef(benpomod_temp)[2]*newx + coef(benpomod_temp)[3]*newx2
  lines(y_out~newx, col="red",lwd=2,lty=3)
  axis(1,c(39,40,41,42),labels=c("","","",""))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.23"), cex=1)
  
  plot.new()

# bottom-dwellers
  plot(log(Benthic$Arate)~Benthic$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab=TeX("Temperature $C^{o}$"),ylab=paste("Juvenile growth (K . L","\U221E",")"),main="Bottom-dwellers")
  
  newx = seq(min(Benthic$Temperature-0.1,na.rm=T),max(Benthic$Temperature+0.1,na.rm=T),by = 0.05)
  newx2 = rep(mean(Benthic$Bentbio),length(newx))
  conf_interval <- predict(botmod, newdata=data.frame(Temperature=newx,Bentbio=newx2), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(botmod)[1] + coef(botmod)[2]*newx + coef(botmod)[3]*newx2
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.61"), cex=1)
  
# flatfishes
  plot(log(Flat$Arate)~Flat$Temperature,yaxt="n",xaxt="n",xlim=c(38.3,42.8),ylim=c(-0.3,6.5),
       xlab="1/cT",ylab="",main="Flatfishes")
  
  newx = seq(min(Flat$Temperature-0.1,na.rm=T),max(Flat$Temperature+0.1,na.rm=T),by = 0.05)
  conf_interval <- predict(flatmod, newdata=data.frame(Temperature=newx), interval="confidence",level = 0.95)
  polygon(c(newx, rev(newx)),c(conf_interval[,2], rev(conf_interval[,3])),col=rgb(0,1,1,alpha=0.5),border=NA) 
  y_out<- coef(flatmod)[1] + coef(flatmod)[2]*newx
  lines(y_out~newx, col="red",lwd=2)
  axis(1,c(39,40,41,42))
  axis(2,c(0,2.3,4.6),labels=c("","",""),las=1)
  text(41.5, 6.3, TeX("$Q_{10}$ = 1.47"), cex=1)
  
  dev.off()