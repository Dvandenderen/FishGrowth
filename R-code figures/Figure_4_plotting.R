#### plot fish growth and temperature, guild and asymptotic size - Figure 4
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)  
  library(merTools)

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  pdf("Growth_guilds.pdf",width=6.9,height=3.56)
  par(mfrow=c(2,3), mar=c(2, 4, 2, 1))
  
# large pelagics
#################
  lPel <- subset(fishes,fishes$grouping == "PEL" & fishes$Linf > 80)
  plot(LArate~Temperature, data=lPel,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="large pelagics")
  
  # get the prediction
  T_across <- seq(min(lPel$Temperature),max(lPel$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("PEL",length(T_across))
  across_LLinf <- rep(mean(lPel$across_LLinf),length(T_across))
  
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("PEL",length(T_across))
  across_LLinf <- rep(mean(lPel$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
 
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.3"), cex=1)
  
# small pelagics
################
  sPel <- subset(fishes,fishes$grouping == "PEL" & fishes$Linf <= 80)
  plot(LArate~Temperature, data=sPel,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="small pelagics")
  
  # get the prediction
  T_across <- seq(min(sPel$Temperature),max(sPel$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("PEL",length(T_across))
  across_LLinf <- rep(mean(sPel$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("PEL",length(T_across))
  across_LLinf <- rep(mean(sPel$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.6"), cex=1)
  
# shark and ray
#################
  shray <- subset(fishes,fishes$grouping == "SHRAY")
  plot(LArate~Temperature, data=shray,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="elasmobranchs")
  
  # get the prediction
  T_across <- seq(min(shray$Temperature),max(shray$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("SHRAY",length(T_across))
  across_LLinf <- rep(mean(shray$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("SHRAY",length(T_across))
  across_LLinf <- rep(mean(shray$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 2.3"), cex=1)
  
# large demersal
#################
  lDem <- subset(fishes,fishes$grouping == "DEM" & fishes$Linf > 80)
  plot(LArate~Temperature, data=lDem,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="large demersals")
  
  # get the prediction
  T_across <- seq(min(lDem$Temperature),max(lDem$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEM",length(T_across))
  across_LLinf <- rep(mean(lDem$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEM",length(T_across))
  across_LLinf <- rep(mean(lDem$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.1"), cex=1)
  
# small demersal
#################
  sDem <- subset(fishes,fishes$grouping == "DEM" & fishes$Linf <= 80)
  plot(LArate~Temperature, data=sDem,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="small demersals")
  
  # get the prediction
  T_across <- seq(min(sDem$Temperature),max(sDem$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEM",length(T_across))
  across_LLinf <- rep(mean(sDem$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEM",length(T_across))
  across_LLinf <- rep(mean(sDem$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.3"), cex=1)
  
# deep-living
#################
  deep <- subset(fishes,fishes$grouping == "DEEP")
  plot(LArate~Temperature, data=deep,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",main="deep-living")
  
  # get the prediction
  T_across <- seq(min(deep$Temperature),max(deep$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEEP",length(T_across))
  across_LLinf <- rep(mean(deep$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yn <- predict(M4, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M4, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
  # get Q10 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  grouping <- rep("DEEP",length(T_across))
  across_LLinf <- rep(mean(deep$across_LLinf),length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  
  # finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(5, 2.3, TeX("$Q_{10}$ = 1.0"), cex=1)
  dev.off()  
  