
#### plot fish asymptotic length and temperature - Figure 4
####################################################
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  load("Processed_files.Rdata")
  M2 <- sumdata[[2]]
  M4 <- sumdata[[4]]
  fishes <- sumdata[[7]]

  setwd("H:/Werk/BP food web model/190319 - run for github/Output") # path for figures
  library(latex2exp)  

# part A - all fishes
  pdf("AsymLength_allfish.pdf",width=4,height=3.56)  
  par(mar=c(2, 4, 2, 1))
  plot(LLinf~Temperature, data=fishes,ylim=c(0.3,2.8),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab=paste("Asymptotic length (cm)"),main="all fish")

  # get the prediction
  meanfish  <- colMeans(M2)
  Te <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  y <- meanfish[1]

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(1))
  for (i  in 1:5000){
    fsamp <- M2[i,]
    uncer[i,] <- fsamp[1]
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)

  # finish plotting
  lines(rep(y,length(Te))~Te,lwd=2, col="blue") 
  lines(rep(ymin,length(Te))~Te,lty=2,col="red")
  lines(rep(ymax,length(Te))~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(1,2),c("10","100"),las=1)
  text(5, 2.7, TeX("$Q_{10}$ = 1.0"), cex=1)
  dev.off()

# part B - fish guilds
  pdf("AsymLength_guilds.pdf",width=6,height=3.56)  # save 6.00 x 3.56 portrait
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))
  meanfish <- colMeans(M4)

# pelagics
################
  Pel <- subset(fishes,fishes$grouping == "PEL")
  plot(LLinf~Temperature, data=Pel,ylim=c(0.3,2.8),las=1,xlim=c(-2,30),cex=1,
      yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
      ylab=paste("Asymptotic length (cm)"),main="pelagics")

  # get the prediction
  Te <- seq(min(Pel$Temperature),max(Pel$Temperature),0.1)
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+ meanfish["groupingPEL"]+
        meanfish["Temperature:groupingPEL"]*Te

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M4[i,]
    uncer[i,] <-  fsamp["Intercept"]+fsamp["Temperature"]*Te+ fsamp["groupingPEL"]+
      fsamp["Temperature:groupingPEL"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)

  # get Q10 
  QTe<- c(10,20)
  yq10 <- fsamp["Intercept"]+fsamp["Temperature"]*QTe+ fsamp["groupingPEL"]+
          fsamp["Temperature:groupingPEL"]*QTe
  10^(yq10[2])/10^(yq10[1])

  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(1,2),c("10","100"),las=1)
  text(5, 2.6, TeX("$Q_{10}$ = 1.1"), cex=1)

# shark and ray
################
  shray <- subset(fishes,fishes$grouping == "SHRAY")
  plot(LLinf~Temperature, data=shray,ylim=c(0.3,2.8),las=1,xlim=c(-2,30),cex=1,
     yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
     ylab=paste("Asymptotic length (cm)"),main="elasmobranchs")

  # get the prediction
  Te <- seq(min(shray$Temperature),max(shray$Temperature),0.1)
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+ meanfish["groupingSHRAY"]+
    meanfish["Temperature:groupingSHRAY"]*Te

  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M4[i,]
    uncer[i,] <-fsamp["Intercept"]+fsamp["Temperature"]*Te+ fsamp["groupingSHRAY"]+
      fsamp["Temperature:groupingSHRAY"]*Te
  }
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  yq10 <- fsamp["Intercept"]+fsamp["Temperature"]*QTe+ fsamp["groupingSHRAY"]+
    fsamp["Temperature:groupingSHRAY"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(1,2),c("10","100"),las=1)
  text(5, 1, TeX("$Q_{10}$ = 0.9"), cex=1)
  
  plot.new()

#  demersal
################
  Dem <- subset(fishes,fishes$grouping == "DEM")
  plot(LLinf~Temperature, data=Dem,ylim=c(0.3,2.8),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab=paste("Asymptotic length (cm)"),main="demersals")
  
  # get the prediction
  Te <- seq(min(lDem$Temperature),max(lDem$Temperature),0.1)
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+ meanfish["groupingDEM"]+
        meanfish["Temperature:groupingDEM"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=5000, ncol=length(Te))
  for (i  in 1:5000){
    fsamp <- M4[i,]
    uncer[i,] <-fsamp["Intercept"]+fsamp["Temperature"]*Te+ fsamp["groupingDEM"]+
      fsamp["Temperature:groupingDEM"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  yq10 <- fsamp["Intercept"]+fsamp["Temperature"]*QTe+ fsamp["groupingDEM"]+
    fsamp["Temperature:groupingDEM"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(1,2),c("10","100"),las=1)
  text(5, 2.6, TeX("$Q_{10}$ = 1.0"), cex=1)

# deep-living
################
  deep <- subset(fishes,fishes$grouping == "DEEP")
  plot(LLinf~Temperature, data=deep,ylim=c(0.3,2.8),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab=paste("Asymptotic length (cm)"),main="deep-living")
  
  # get the prediction
  Te <- seq(min(deep$Temperature),max(deep$Temperature),0.1)
  y  <- meanfish["Intercept"]+meanfish["Temperature"]*Te
  
  # get uncertainty (min and max of all resampled estimates)
  uncer <- matrix(data=NA,nrow=1000, ncol=length(Te))
  for (i  in 1:1000){
    fsamp <- M4[i,]
    uncer[i,] <- fsamp["Intercept"]+fsamp["Temperature"]*Te
  } 
  ymin <- apply(uncer,2,FUN=min)
  ymax <- apply(uncer,2,FUN=max)
  
  # get Q10 
  QTe<- c(10,20)
  yq10 <- fsamp["Intercept"]+fsamp["Temperature"]*QTe
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  lines(y~Te,lwd=2, col="blue") 
  lines(ymin~Te,lty=2,col="red")
  lines(ymax~Te,lty=2,col="red")
  axis(1,c(0,15,30))
  axis(2,c(1,2),c("10","100"),las=1)
  text(5, 2.6, TeX("$Q_{10}$ of 0.6"), cex=1)

  dev.off()  
