
## plot fish growth and temperature - Figure 3
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)  
  library(merTools)

# get 95% CI for coefficients for M1 model
  M1_prof <- profile(M1)
  M1_unc <- confint(M1_prof,level=0.95)

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
# part A - all fishes
  pdf("Growth_allfish.pdf",width=6.9,height=3.56)  
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
  plot(LArate~Temperature, data=fishes,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A",col="grey")

# get the across species prediction from M1
  T_across <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg)
  yn <- predict(M1, newdata = newdat, re.form = NA)
  
  lines(yn~T_across,col="blue",lwd=2)

# calculate the 95% confidence interval
  pred <- bootMer(M1, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="blue")
  lines(CI.lower~T_across,lty=2,col="blue")
  
# get Q10 between species 
  T_across <- c(10,20)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg)
  yq10 <- predict(M1, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  yq10_lower <- M1_unc[4,1]+M1_unc[6,1]*T_across
  10^(yq10_lower[2])/10^(yq10_lower[1])
  yq10_upper <- M1_unc[4,2]+M1_unc[6,2]*T_across
  10^(yq10_upper[2])/10^(yq10_upper[1])
  
# get Q10 within species
  T_within <- c(10,20)
  Name <- rep(NA,length(T_within))
  uniReg <- rep(NA, length(T_within))
  T_across <- rep(0,length(T_within))
  newdat <- data.frame(T_within,Name,T_within,uniReg)
  yq10 <- predict(M1, newdata = newdat, re.form = NA)
  10^(yq10[2])/10^(yq10[1])
  yq10_lower <- M1_unc[4,1]+M1_unc[5,1]*T_within
  10^(yq10_lower[2])/10^(yq10_lower[1])
  yq10_upper <- M1_unc[4,2]+M1_unc[5,2]*T_within
  10^(yq10_upper[2])/10^(yq10_upper[1])
  
# finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  text(28, 2.4, "(a)", cex=1)
  
# part B - within versus between species response with constant slope
  plot(LArate~Temperature, data=fishes,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A", col="white")
  
# get the within species prediction from M2 
  randomeff <- ranef(M2)$Name
  unifish <- rownames(randomeff)
  for (j in 1:771){
    dd <- subset(fishes,fishes$Name == unifish[j])
    T_within <- c(min(dd$T_within),max(dd$T_within))
    if (length(unique(T_within)) > 1){
      T_across <- rep(dd$T_across[1],2)
      yn <- fixef(M2)[1] + randomeff[j,1] + fixef(M2)[3] * T_across + fixef(M2)[2]*T_within + randomeff[j,2] * T_within 
      Tall <- c(min(dd$Temperature),max(dd$Temperature))
      lines(yn~Tall,col="grey",lwd=1)
    }
  }
  
# add metabolic prediction
  T_across <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  A_0  <- 3
  Amin <- A_0*2.5^((min(T_across)-0)/10)
  Amax <- A_0*2.5^((max(T_across)-0)/10)
  ymet <- c(log10(Amin),log10(Amax))
  Tmet <- c(min(T_across),max(T_across))
  lines(ymet~Tmet, col="red", lwd=2,lty=1)  
  
# get the across species prediction from M2
  T_across <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg)
  yn <- predict(M2, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="blue",lwd=2)
  
# calculate the 95% confidence interval
  pred <- bootMer(M2, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  lines(CI.upper~T_across,lty=2,col="blue")
  lines(CI.lower~T_across,lty=2,col="blue")
  
# finish plotting
  axis(1,c(0,15,30))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  
  legend(-2,log10(300), legend=c("within", "between", "MTE"),
         col=c("grey","blue","red"), lty=c(1,1,1),lwd=c(2,2,2), cex=0.8,
         box.lty=0)
  text(28, 2.4, "(b)", cex=1)
  
  dev.off()