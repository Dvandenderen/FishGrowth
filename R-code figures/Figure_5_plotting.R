
#### plot fish growth x guild and temperature illustration - Figure 6
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp) 
  
  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  pdf("Guild_illustration_growth.pdf",width=4,height=5.2)  
  
# create a plot with the metabolic prediction
  temp <- c(0,30)
  metl <- c(log10(3),log10(3*2.5^3))
  plot(metl~temp, lty=5,lwd=1,type="l", xlim=c(-2,30), ylim=c(0.4,1.7),col="black",
       xaxt="n",yaxt="n", xlab=TeX("$Temperature$ ^{o}$C"),ylab="Growth coef. A")
  
# predict the guild effect 
  Name <- rep(NA,length(2))
  uniReg <- rep(NA, length(2))
  T_within <- rep(0,length(2))
  across_LLinf <- rep(log10(100),length(2)) # for 100 cm fish
  
# 100 cm pelagic
  grouping <- rep("PEL",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf >100])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#ef3b2c",lwd=2)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
# 100 cm elasmobranchs
  grouping <- rep("SHRAY",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="SHRAY" & fishes$Linf >100])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="black",lwd=2)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
# 100 cm demersal
  grouping <- rep("DEM",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf >100])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#08519c",lwd=2)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
# 100 cm deep-living
  grouping <- rep("DEEP",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf >100])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf >100])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#bdbdbd",lwd=2)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- rep(log10(30),length(2)) # for 30 cm fish
  
# 30 cm pelagic
  grouping <- rep("PEL",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="PEL" & fishes$Linf <30])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#ef3b2c",lwd=2,lty=5)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
# 30 cm demersal
  grouping <- rep("DEM",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEM" & fishes$Linf <30])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#08519c",lwd=2,lty=5)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
# 30 cm deep-living
  grouping <- rep("DEEP",length(2))
  nbmin <- min(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf <30])
  nbmax <- max(fishes$Temperature[fishes$grouping=="DEEP" & fishes$Linf <30])
  T_across <- c(nbmin,nbmax)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M5, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="#bdbdbd",lwd=2,lty=5)
  
  T_across <- c(10,20)
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  axis(1,c(0,15,30))
  axis(2,c(log10(2),log10(4),log10(8),log10(16),log10(32)),c("2","4","8","16","32"),las=1)
  
  legend(0,log10(45), legend=c("pelagics", "demersals", "elasmobranchs","deep-living"),
         col=c("#ef3b2c","#08519c","black","#bdbdbd"), lty=c(1,1,1,1),lwd=c(2,2,2,2), cex=0.8,
         box.lty=0)
  dev.off()