
## plot fish growth and temperature - Figure 4
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
       ylab="Growth coef. A")

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
  lines(CI.upper~T_across,lty=2,col="red")
  lines(CI.lower~T_across,lty=2,col="red")
  
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
  #text(5, 2.3, TeX("$Q_{10}$ = 1.4"), cex=1)
 
# part B - within species response with random slope and intercept

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
  lines(c(1.28,1.28),c(1,87),col="red",lty=2)
  lines(c(1.48,1.48),c(1,87),col="red",lty=2)

  dev.off()
  