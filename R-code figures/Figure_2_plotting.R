
#### plot fish growth and temperature - Figure 2
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  load("Processed_files.Rdata")
  M1 <- sumdata[[1]]
  M3 <- sumdata[[3]]
  fishes <- sumdata[[7]]

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output") # path for figures
  library(latex2exp)  

# part A - all fishes
  pdf("Growth_allfish.pdf",width=4,height=3.56)  
  par(mar=c(2, 4, 2, 1))
  plot(LArate~Temperature, data=fishes,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="all fish")

  # get the prediction from M1
  meanfish  <- colMeans(M1)
  Te <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  y <- meanfish[1]+meanfish[2]*Te

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M1[i,]
    uncer[i,] <- fsamp[1]+fsamp[2]*Te
    } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  yq10 <- meanfish[1]+meanfish[2]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.4"), cex=1)
  dev.off()
  
# part B - fish guilds
  pdf("Growth_guilds.pdf",width=6,height=3.56)
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))
  
  # get the prediction from M3
  meanfish <- colMeans(M3)

# large pelagics
#################
  lPel <- subset(fishes,fishes$grouping == "PEL" & fishes$Linf > 80)
  plot(LArate~Temperature, data=lPel,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="large pelagics")
  
  # get the prediction
  Te <- seq(min(lPel$Temperature),max(lPel$Temperature),0.1)
  Li <- rep(mean(lPel$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
        meanfish["Temperature:groupingPEL"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
                 fsamp["groupingPEL"]+fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li + 
                 fsamp["Temperature:groupingPEL"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)

  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(lPel$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <- meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
          meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li + 
          meanfish["Temperature:groupingPEL"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
   # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.4"), cex=1)
  
# small pelagics
################
  sPel <- subset(fishes,fishes$grouping == "PEL" & fishes$Linf <= 80)
  plot(LArate~Temperature, data=sPel,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="small pelagics")
  
  # get the prediction
  Te <- seq(min(sPel$Temperature),max(sPel$Temperature),0.1)
  Li <- rep(mean(sPel$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
        meanfish["Temperature:groupingPEL"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
      fsamp["groupingPEL"]+fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li + 
      fsamp["Temperature:groupingPEL"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)

  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(sPel$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <- meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
          meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li + 
          meanfish["Temperature:groupingPEL"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.7"), cex=1)
  
# shark and ray
#################
  shray <- subset(fishes,fishes$grouping == "SHRAY")
  plot(LArate~Temperature, data=shray,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="elasmobranchs")
  
  # get the prediction
  Te <- seq(min(shray$Temperature),max(shray$Temperature),0.1)
  Li <- rep(mean(shray$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
        meanfish["Temperature:groupingSHRAY"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <-fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
                fsamp["groupingSHRAY"]+fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li + 
                fsamp["Temperature:groupingSHRAY"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(shray$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <- meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
          meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li + 
          meanfish["Temperature:groupingSHRAY"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 2.1"), cex=1)
  
# large demersal
#################
  lDem <- subset(fishes,fishes$grouping == "DEM" & fishes$Linf > 80)
  plot(LArate~Temperature, data=lDem,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="large demersals")
  
  # get the prediction
  Te <- seq(min(lDem$Temperature),max(lDem$Temperature),0.1)
  Li <- rep(mean(lDem$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
        meanfish["Temperature:groupingDEM"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <-fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
      fsamp["groupingDEM"]+fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li + 
      fsamp["Temperature:groupingDEM"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(lDem$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <- meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
          meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li + 
          meanfish["Temperature:groupingDEM"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.2"), cex=1)
  
# small demersal
#################
  sDem <- subset(fishes,fishes$grouping == "DEM" & fishes$Linf <= 80)
  plot(LArate~Temperature, data=sDem,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="small demersals")
  
  # get the prediction
  Te <- seq(min(sDem$Temperature),max(sDem$Temperature),0.1)
  Li <- rep(mean(sDem$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
        meanfish["Temperature:groupingDEM"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <-fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
      fsamp["groupingDEM"]+fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li + 
      fsamp["Temperature:groupingDEM"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(sDem$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <-  meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
           meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li + 
           meanfish["Temperature:groupingDEM"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.4"), cex=1)
  
# deep-living
#################
  deep <- subset(fishes,fishes$grouping == "DEEP")
  plot(LArate~Temperature, data=deep,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="deep-living")
  
  # get the prediction
  Te <- seq(min(deep$Temperature),max(deep$Temperature),0.1)
  Li <- rep(mean(deep$LLinf),length(Te))
  tz <- rep(0,length(Te))
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
        meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M3[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te+fsamp["LLinf"]*Li+
      fsamp["Tzero"]*tz + fsamp["Temperature:LLinf"]*Te*Li
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
 
  # get Q10 
  QTe<- c(10,20)
  Li <- rep(mean(deep$LLinf),length(QTe))
  tz <- rep(0,length(QTe))
  yq10 <- meanfish["Intercept"]+meanfish["Temperature"]*QTe+meanfish["LLinf"]*Li+
          meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*QTe*Li
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.2"), cex=1)
  dev.off()  
  