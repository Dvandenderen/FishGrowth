
#### plot fish growth and food availability for pelagic fish - supplement
####################################################
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  load("Processed_files.Rdata")
  M5 <- sumdata[[5]]
  fishes <- sumdata[[7]]
  
  setwd("H:/Werk/BP food web model/190319 - run for github/Output") # path for figures
  library(latex2exp)  
  
  pdf("Suppl_foodPel.pdf",width=4,height=4)  
  
  # get the prediction from M5
  meanfish <- colMeans(M5)

  # select the data  
  sPel <-   subset(fishes,fishes$grouping =="PEL" & fishes$Linf < 50)
  sPel <- subset(sPel,!(is.na(sPel$Zoobio)))

  plot(LArate~Zoobio,data=sPel,ylim=c(-0.4,2.5),col="black",lty=3,xlim=c(0,3),
       cex=0.75, yaxt="n",xaxt="n", xlab=TeX("Zoop. biomass (gr C $m^{-2}$)"),
       ylab="Growth coef. A")
  
  # get the prediction
  Bb <- seq(min(sPel$Zoobio),max(sPel$Zoobio),0.01)
  Li <- rep(mean(sPel$LLinf),length(Bb))
  tz <- rep(0,length(Bb))
  Te <- rep(mean(sPel$Temperature),length(Bb))
  
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te + meanfish["LLinf"]*Li +
        meanfish["Tzero"]*tz + meanfish["LLinf:Temperature"]*Te*Li 

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Bb))
  for (i  in 1:5000){
    fsamp <- M5[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te + fsamp["LLinf"]*Li +
                 fsamp["Tzero"]*tz + fsamp["LLinf:Temperature"]*Te*Li 
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  lines(y~Bb,lwd=2, col="blue") 
  lines(ymin~Bb,lty=2,col="red")
  lines(ymax~Bb,lty=2,col="red")
  axis(1,c(0,1.5,3))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
  dev.off()
  