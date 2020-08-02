
#### plot growth and food - Figure 6
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(latex2exp)

# get random effect of ecoregion
  reff <- ranef(M4)
  ecoreg <- reff$uniReg
  ecoreg$ecreg <- rownames(ecoreg)
  rownames(ecoreg) <- NULL
  
# add Zoobio, zooprod, NPP and temperature
  ecoreg<-cbind(ecoreg,fishes[match(ecoreg$ecreg,fishes$uniReg),c(14,15,19,21)])
  
# calculate number of observations and unique species observations per region
  newReg <- table(fishes$uniReg)
  newReg <- as.data.frame(newReg) 
  newReg$Freq <- as.numeric(newReg$Freq)
  newReg$Specs <- 0
  for (i in 1:nrow(newReg)){
    nn <- subset(fishes,fishes$uniReg == newReg$Var1[i])
    newReg$Specs[i] <-  length(unique(nn$Name))
  }

# add obs and unique species obs per region
  ecoreg<-cbind(ecoreg,newReg[match(ecoreg$ecreg,newReg$Var1),c(2,3)])

# adjust some names
  colnames(ecoreg)[7] <-"obs"
  colnames(ecoreg)[1] <-"intercept"

# select all regions with more than 10 species
  eco_sub  <- subset(ecoreg, ecoreg$Specs > 10)

# no NPP data for black sea, hence remove for NPP analysis
  eco_zero <- subset(eco_sub,eco_sub$NPProd >0) 

# no fit the models and plot the data
  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output")
  pdf("Growth_food.pdf",width=6.9,height=3)
  par(mfrow=c(1,3), mar=c(6, 4, 2, 1))
  
  plot(eco_zero$intercept ~ eco_zero$NPProd,ylab="Ecoregion random intercept growth variation",
       las=1,xlab=TeX("NPP (g  C / m$^2$ / day)"))  
  axis(1,c(0.5,1,1.5,2))
  text(0.4, 0.2, "(a)", cex=1)
  mod1 <- lm(intercept ~ NPProd, data = eco_zero)
  summary(mod1)
  
  newx <- seq(min(eco_zero$NPProd), max(eco_zero$NPProd), length.out=100)
  preds <- predict(mod1, newdata = data.frame(NPProd=newx), 
                   interval = 'confidence')
  
  abline(mod1)
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  plot(eco_sub$intercept ~ eco_sub$Zoobio, ylab="",xlab=TeX("Zooplankton biomass (g  C / m$^2$)"),yaxt="n") 
  axis(1,c(0.5,1,1.5,2))
  axis(2,c(-.2,-.1,0,.1,.2),c("","","","",""))
  text(0.6, 0.2, "(b)", cex=1)
  mod2 <- lm(intercept ~ Zoobio, data = eco_sub)
  summary(mod2)
  
  newx <- seq(min(eco_sub$Zoobio), max(eco_sub$Zoobio), length.out=100)
  preds <- predict(mod2, newdata = data.frame(Zoobio=newx), 
                   interval = 'confidence')
  
  abline(mod2)
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  plot(eco_sub$intercept ~ eco_sub$Zooprod,ylab="",xlab=TeX("ZP (g  C / m$^2$ / day)"),yaxt="n",xaxt="n",xlim=c(0.005,0.08)) 
  axis(1,c(0.01,0.04,0.08))
  axis(2,c(-.2,-.1,0,.1,.2),c("","","","",""))
  text(0.01, 0.2, "(c)", cex=1)
  mod3 <- lm(intercept ~ Zooprod, data=eco_sub)
  summary(mod3)
  
  newx <- seq(min(eco_sub$Zooprod), max(eco_sub$Zooprod), length.out=100)
  preds <- predict(mod3, newdata = data.frame(Zooprod=newx), 
                   interval = 'confidence')
  abline(mod3)
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  dev.off()