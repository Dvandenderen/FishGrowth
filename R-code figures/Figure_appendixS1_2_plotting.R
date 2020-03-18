
## plot within species variation in the slope of the between species effect - Figure S1-2
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)  
  library(merTools)

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  
# part A - within species response with random slope and intercept
  pdf("Within_growth_suppl.pdf",width=6.9,height=3.56)  
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))

# get 95% CI for coefficients for M3 model
  M3_prof <- profile(M3)
  M3_unc <- confint(M3_prof,level=0.95)

# get random effect 
  reff <- ranef(M3_LMER)
  reff <- reff$Name
  reff$Name <- rownames(reff)
  rownames(reff) <- c()

# simulate 95 CI of random effects from merMod object posterior distributions 
  resamp <- REsim(M3_LMER, n.sims = 1500)
  resamp$Twithin_lower <- resamp$mean-1.96*resamp$sd
  resamp$Twithin_upper <- resamp$mean+1.96*resamp$sd
  resamp <- subset(resamp,resamp$term =="T_within" & resamp$groupFctr =="Name")

# bind data together
  dat_within <- cbind(reff,resamp[match(reff$Name,resamp$groupID),c(7:8)])
  dat_within$FE_Twithin <- fixef(M3)[2]
  dat_within$FE_Twithin_lower <- M3_unc[7,1]
  dat_within$FE_Twithin_upper <- M3_unc[7,2]

# estimate Q10 per species + 95% CI 
  q10within <- as.data.frame(matrix(data=NA, ncol=5,nrow=nrow(dat_within)))
  colnames(q10within) <- c("Name","q10","q10_lower","q10_upper","subset")
  for (j in 1:771){
    dd <- subset(fishes,fishes$Name ==  dat_within$Name[j])
    q10within[j,5] <- ((nrow(dd)>1) & (max(dd$T_within)-min(dd$T_within) > 5))
    within_RE <- dat_within[j,"T_within"]
    within_FE <- dat_within[j,"FE_Twithin"]
    within_low_RE <- dat_within[j,"Twithin_lower"]
    within_low_FE <- dat_within[j,"FE_Twithin_lower"]
    within_high_RE <- dat_within[j,"Twithin_upper"]
    within_high_FE <- dat_within[j,"FE_Twithin_upper"]
    dif <- c(-5,5)
    y2 <- within_RE * dif + within_FE*dif
    y2_low <- within_low_RE * dif + within_low_FE * dif
    y2_high <- within_high_RE * dif + within_high_FE * dif
    xd <- dif[2]-dif[1]
    q10within[j,2] <- 10^((y2[2]-y2[1])/xd*10)
    q10within[j,3] <- 10^((y2_low[2]-y2_low[1])/xd*10)
    q10within[j,4] <- 10^((y2_high[2]-y2_high[1])/xd*10)
    q10within[j,1] <- dat_within$Name[j]
  }
  
  colMeans(q10within[2:4])

  q10within <- q10within[order(q10within$q10),]
  subq10with <- subset(q10within,q10within$subset =="TRUE")
  
  plot(subq10with$q10,c(1:nrow(subq10with)),xlim=c(0,3.5),ylab="Species", 
       xlab=TeX("$Q_{10}$"),las=1,yaxt="n",xaxt="n")
  for (i in 1:nrow(subq10with)){
    lines(c(subq10with$q10_lower[i],subq10with$q10_upper[i]),c(i,i),lwd=0.25)
  }
  points(subq10with$q10,c(1:nrow(subq10with)),pch=16,col="grey")
  
  axis(1,c(0,1,2,3))
  axis(2,c(1,40,80),las=1)
  lines(c(1.38,1.38),c(1,87),col="blue")
  lines(c(1.28,1.28),c(1,87),col="blue",lty=2)
  lines(c(1.48,1.48),c(1,87),col="blue",lty=2)
  lines(c(2.5,2.5),c(1,87),col="red",lty=1)
  text(0.2, 82, "(a)", cex=1)
  
# part B - within versus between species response with constant slope
  plot(LArate~Temperature, data=fishes,ylim=c(-0.4,2.5),las=1,xlim=c(-2,30),cex=1,
       yaxt="n",xaxt="n", xlab=TeX("Temperature ($^o$C)"),
       ylab="Growth coef. A", col="white")
  
  # get the within species prediction from M1 
  randomeff <- ranef(M3)$Name
  unifish <- rownames(randomeff)
  for (j in 1:771){
    dd <- subset(fishes,fishes$Name == unifish[j])
    T_within <- c(min(dd$T_within),max(dd$T_within))
    if (length(unique(T_within)) > 1){
      T_across <- rep(dd$T_across[1],2)
      yn <- fixef(M3)[1] + randomeff[j,1] + fixef(M3)[3] * T_across + fixef(M3)[2]*T_within + randomeff[j,2] * T_within 
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
  
  # get the across species prediction from M1
  T_across <- seq(min(fishes$Temperature),max(fishes$Temperature),0.1)
  Name <- rep(NA,length(T_across))
  uniReg <- rep(NA, length(T_across))
  T_within <- rep(0,length(T_across))
  newdat <- data.frame(T_across,Name,T_within,uniReg)
  yn <- predict(M3, newdata = newdat, re.form = NA)
  lines(yn~T_across,col="blue",lwd=2)
  
  # calculate the 95% confidence interval
  pred <- bootMer(M3, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
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