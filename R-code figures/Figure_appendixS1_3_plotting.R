
## plot within species variation in the slope of the between species effect - Figure S1-3
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)  
  library(merTools)

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  
# part A - within species response with random slope and intercept
  pdf("Within_growth_suppl.pdf",width=5,height=4.2)  
  par(mfrow=c(1,1), mar=c(4, 4, 2, 1))

# get random effect per species
  reff <- ranef(M2)
  reff <- reff$Name
  reff$Name <- rownames(reff)
  rownames(reff) <- c()  
  
# add fixed within-species effect
  reff$FE_Twithin <- fixef(M2)[2]

# estimate Q10 per species + 95% CI 
  q10within <- as.data.frame(matrix(data=NA, ncol=3,nrow=nrow(reff)))
  colnames(q10within) <- c("Name","q10","subset")
  for (j in 1:771){
    dd <- subset(fishes,fishes$Name ==  reff$Name[j])
    q10within[j,3] <- ((nrow(dd)>1) & (max(dd$T_within)-min(dd$T_within) > 5))
    within_RE <- reff[j,"T_within"]
    within_FE <- reff[j,"FE_Twithin"]
    dif <- c(-5,5)
    y2 <- within_RE * dif + within_FE*dif
    xd <- dif[2]-dif[1]
    q10within[j,2] <- 10^((y2[2]-y2[1])/xd*10)
    q10within[j,1] <- reff$Name[j]
  }
  
  colMeans(q10within[2])

  q10within <- q10within[order(q10within$q10),]
  subq10with <- subset(q10within,q10within$subset =="TRUE")
  
  plot(subq10with$q10,c(1:nrow(subq10with)),xlim=c(0,3),ylab="Species", 
       xlab=TeX("$Q_{10}$"),las=1,yaxt="n",xaxt="n",pch=16, col="grey")
  axis(1,c(0,1,2,3))
  axis(2,c(1,40,80),las=1)
  lines(c(1.38,1.38),c(1,87),col="blue")
  lines(c(1.28,1.28),c(1,87),col="blue",lty=2)
  lines(c(1.48,1.48),c(1,87),col="blue",lty=2)
  lines(c(2.5,2.5),c(1,87),col="red",lty=1)

  dev.off()