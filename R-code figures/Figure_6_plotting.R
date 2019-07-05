
#### plot fish growth and food availability - Figure 5
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  load("Processed_files.Rdata")
  M6 <- sumdata[[6]]
  fishes <- sumdata[[7]]
  
  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output") # path for figures
  library(latex2exp)  
  
  pdf("Growth_food.pdf",width=6,height=3.56)  
 
  # get the prediction from M6
  meanfish <- colMeans(M6)

  #### get the plot
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))

# < 15 degrees and < 30 cm   
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Benthbio)))
  sDem <- subset(sDem,sDem$Linf <= 30)
  sDem <- subset(sDem,sDem$Temperature <= 15)
  
  plot(LArate~Bentbio,data=sDem,ylim=c(-0.4,2.5),col="black",lty=3,xlim=c(0,3),
       cex=1, yaxt="n",xaxt="n", xlab=TeX("Benthic biomass (gr C $m^{-2}$)"),
       ylab="Growth coef. A",main="< 15 C, L <30 cm")
  
  # get the prediction
  Bb <- seq(min(sDem$Bentbio),max(sDem$Bentbio),0.01)
  Li <- rep(mean(sDem$LLinf),length(Bb))
  tz <- rep(0,length(Bb))
  Te <- rep(mean(sDem$Temperature),length(Bb))
  
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["Bentbio"]*Bb+
        meanfish["LLinf"]*Li + meanfish["Tzero"]*tz + meanfish["Temperature:Bentbio"]*Te*Bb + 
        meanfish["Temperature:LLinf"]*Te*Li + meanfish["Bentbio:LLinf"]*Bb*Li +
        meanfish["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Bb))
  for (i  in 1:5000){
    fsamp <- M6[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["Bentbio"]*Bb+
      fsamp["LLinf"]*Li + fsamp["Tzero"]*tz + fsamp["Temperature:Bentbio"]*Te*Bb + 
      fsamp["Temperature:LLinf"]*Te*Li + fsamp["Bentbio:LLinf"]*Bb*Li +
      fsamp["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  lines(y~Bb,lwd=2, col="blue") 
  lines(ymin~Bb,lty=2,col="red")
  lines(ymax~Bb,lty=2,col="red")
  axis(1,c(0,1.5,3))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
# < 15 degrees and > 30 cm   
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Bentbio)))
  sDem <- subset(sDem,sDem$Linf > 30)
  sDem <- subset(sDem,sDem$Temperature <= 15)
  
  plot(LArate~Bentbio,data=sDem,ylim=c(-0.4,2.5),col="black",lty=3,xlim=c(0,3),
       cex=1, yaxt="n",xaxt="n", xlab=TeX("Benthic biomass (gr C $m^{-2}$)"),
       ylab="Growth coef. A",main="< 15 C, L >30 cm")
  
  # get the prediction
  Bb <- seq(min(sDem$Bentbio),max(sDem$Bentbio),0.01)
  Li <- rep(mean(sDem$LLinf),length(Bb))
  tz <- rep(0,length(Bb))
  Te <- rep(mean(sDem$Temperature),length(Bb))
  
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["Bentbio"]*Bb+
    meanfish["LLinf"]*Li + meanfish["Tzero"]*tz + meanfish["Temperature:Bentbio"]*Te*Bb + 
    meanfish["Temperature:LLinf"]*Te*Li + meanfish["Bentbio:LLinf"]*Bb*Li +
    meanfish["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Bb))
  for (i  in 1:5000){
    fsamp <- M6[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["Bentbio"]*Bb+
      fsamp["LLinf"]*Li + fsamp["Tzero"]*tz + fsamp["Temperature:Bentbio"]*Te*Bb + 
      fsamp["Temperature:LLinf"]*Te*Li + fsamp["Bentbio:LLinf"]*Bb*Li +
      fsamp["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  lines(y~Bb,lwd=2, col="blue") 
  lines(ymin~Bb,lty=2,col="red")
  lines(ymax~Bb,lty=2,col="red")
  axis(1,c(0,1.5,3))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
  plot.new()
  
# > 15 degrees and < 30 cm   
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Bentbio)))
  sDem <- subset(sDem,sDem$Linf <= 30)
  sDem <- subset(sDem,sDem$Temperature > 15)
  
  plot(LArate~Bentbio,data=sDem,ylim=c(-0.4,2.5),col="black",lty=3,xlim=c(0,3),
       cex=1, yaxt="n",xaxt="n", xlab=TeX("Benthic biomass (gr C $m^{-2}$)"),
       ylab="Growth coef. A",main=">15 C, L <30 cm")
  
  # get the prediction
  Bb <- seq(min(sDem$Bentbio),max(sDem$Bentbio),0.01)
  Li <- rep(mean(sDem$LLinf),length(Bb))
  tz <- rep(0,length(Bb))
  Te <- rep(mean(sDem$Temperature),length(Bb))
  
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["Bentbio"]*Bb+
        meanfish["LLinf"]*Li + meanfish["Tzero"]*tz + meanfish["Temperature:Bentbio"]*Te*Bb + 
        meanfish["Temperature:LLinf"]*Te*Li + meanfish["Bentbio:LLinf"]*Bb*Li +
        meanfish["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Bb))
  for (i  in 1:5000){
    fsamp <- M6[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["Bentbio"]*Bb+
                 fsamp["LLinf"]*Li + fsamp["Tzero"]*tz + fsamp["Temperature:Bentbio"]*Te*Bb + 
                 fsamp["Temperature:LLinf"]*Te*Li + fsamp["Bentbio:LLinf"]*Bb*Li +
                 fsamp["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  lines(y~Bb,lwd=2, col="blue") 
  lines(ymin~Bb,lty=2,col="red")
  lines(ymax~Bb,lty=2,col="red")
  axis(1,c(0,1.5,3))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
# > 15 degrees and > 30 cm   
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Bentbio)))
  sDem <- subset(sDem,sDem$Linf > 30)
  sDem <- subset(sDem,sDem$Temperature > 15)
  
  plot(LArate~Bentbio,data=sDem,ylim=c(-0.4,2.5),col="black",lty=3,xlim=c(0,3),
       cex=1, yaxt="n",xaxt="n", xlab=TeX("Benthic biomass (gr C $m^{-2}$)"),
       ylab="Growth coef. A",main=">15 C, L >30 cm")
  
  # get the prediction
  Bb <- seq(min(sDem$Bentbio),max(sDem$Bentbio),0.01)
  Li <- rep(mean(sDem$LLinf),length(Bb))
  tz <- rep(0,length(Bb))
  Te <- rep(mean(sDem$Temperature),length(Bb))
  
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["Bentbio"]*Bb+
    meanfish["LLinf"]*Li + meanfish["Tzero"]*tz + meanfish["Temperature:Bentbio"]*Te*Bb + 
    meanfish["Temperature:LLinf"]*Te*Li + meanfish["Bentbio:LLinf"]*Bb*Li +
    meanfish["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=1000, ncol=length(Bb))
  for (i  in 1:1000){
    fsamp <- M6[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["Bentbio"]*Bb+
      fsamp["LLinf"]*Li + fsamp["Tzero"]*tz + fsamp["Temperature:Bentbio"]*Te*Bb + 
      fsamp["Temperature:LLinf"]*Te*Li + fsamp["Bentbio:LLinf"]*Bb*Li +
      fsamp["Temperature:Bentbio:LLinf"]*Te*Bb*Li
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  lines(y~Bb,lwd=2, col="blue") 
  lines(ymin~Bb,lty=2,col="red")
  lines(ymax~Bb,lty=2,col="red")
  axis(1,c(0,1.5,3))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
  plot.new()
  
  dev.off()