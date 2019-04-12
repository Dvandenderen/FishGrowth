
#### plot fish growth and temperature - Figure 3
####################################################
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  load("Processed_files.Rdata")
  M3 <- sumdata[[3]]
  fishes <- sumdata[[7]]
  
  setwd("H:/Werk/BP food web model/190319 - run for github/Output") # path for figures
  library(latex2exp)  

  pdf("Illustration_growth.pdf",width=4,height=5.2)  
  
# get the prediction from M3
  meanfish  <- colMeans(M3)
  
# get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","deep","dem","shray")
    
# calculate temp effect + Q10 for asymptoic length 100 cm fish
  Te <- seq(0,30)
  Li <- rep(log10(100),length(Te))
  tz <- rep(0,length(Te))

  ypel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
           meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,1] <- (10^(ypel[31])/10^(ypel[1]))^(10/(30-0))
  
  ydeep  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
            meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,1] <- (10^(ydeep[31])/10^(ydeep[1]))^(10/(30-0))
  
  ydem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
           meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,1] <- (10^(ydem[31])/10^(ydem[1]))^(10/(30-0))
  
  yshray  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
             meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
             meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,1] <- (10^(yshray[31])/10^(yshray[1]))^(10/(30-0))
  
# calculate temp effect + Q10 for asymptoic length 30 cm fish 
  Te <- seq(0,30)
  Li <- rep(log10(30),length(Te))
  tz <- rep(0,length(Te))
  
  yspel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
            meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
            meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,2] <- (10^(yspel[31])/10^(yspel[1]))^(10/(30-0))
  
  ysdeep  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
             meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,2] <- (10^(ysdeep[31])/10^(ysdeep[1]))^(10/(30-0))
  
  ysdem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,2] <- (10^(ysdem[31])/10^(ysdem[1]))^(10/(30-0))
  
  ysshray  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,2] <- (10^(ysshray[31])/10^(ysshray[1]))^(10/(30-0))
  
# now plot the temperature effects for the different guilds
  
  # first add metabolic Q10 of 2.5 from A = 3 at 0 degrees celsius
  temp <- c(0,30)
  metl <- c(log10(3),log10(3*2.5^3))
  plot(metl~temp, lty=5,lwd=1,type="l", xlim=c(-2,30), ylim=c(0.4,1.7),col="black",
       xaxt="n",yaxt="n", xlab=TeX("$Temperature$ ^{o}$C"),ylab="Growth coef. A")
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf >100])
  lines(ypel[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="#ef3b2c", lty=1,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf >100])
  lines(ydeep[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="#bdbdbd", lty=1,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf >100])
  lines(ydem[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax], 
        col="#08519c", lty=1,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf >100])
  lines(yshray[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="black", lty=1,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf <30])
  lines(yspel[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="#ef3b2c", lty=5,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf <30])
  lines(ysdeep[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="#bdbdbd", lty=5,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf<30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf <30])
  lines(ysdem[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax], 
        col="#08519c", lty=5,lwd=3)
  
  nbmin <- min(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf <30])
  lines(ysshray[Te>nbmin & Te< nbmax]~Te[Te>nbmin & Te< nbmax],
        col="black", lty=5,lwd=3)
  
  axis(1,c(0,15,30))
  axis(2,c(log10(2),log10(4),log10(8),log10(16),log10(32)),c("2","4","8","16","32"),las=1)
  
  legend(0,log10(45), legend=c("pelagics", "demersals", "elasmobranchs","deep-living"),
         col=c("#ef3b2c","#08519c","black","#bdbdbd"), lty=c(1,1,1,1),lwd=c(2,2,2,2), cex=0.8,
         box.lty=0)
dev.off()
  


