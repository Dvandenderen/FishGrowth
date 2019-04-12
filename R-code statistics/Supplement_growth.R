
### supplement to analyze robustness of patterns with different data extraction methods
####################################################

### tzero -1 and +1
########################
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Processing_fish_data.R")
  
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Resample.R")
  
  # use all observations with tzero -1 and +1
  fishes <- subset(datFish,datFish$Tzero >= -1 & datFish$Tzero <= 1)
  
  # derive ambient temperature
  fishes$Temperature <- -100
  for(i in 1:nrow(fishes)) {
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$Stemp[i] }    # temperature in first 100 meter 
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$Btemp[i]} # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
  }
  
  # group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" ] <- "PEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic" | fishes$Func_group == "Reef"] <- "DEM"
  
  # get log10 
  fishes$LLinf  <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)

  # model selection
  modFish <- matrix(data=NA,ncol=8,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8")
  
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * grouping * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*grouping + grouping * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + grouping*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * grouping + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*grouping +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + grouping +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * grouping + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + grouping + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8)
    modFish[i,] <- out$AIC
  }

  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  M3 <- matrix(data=NA,ncol=18,nrow=5000)
  colnames(M3) <- c("Intercept","Temperature","groupingDEM","groupingPEL","groupingSHRAY","LLinf",
                    "Tzero","Temperature:groupingDEM","Temperature:groupingPEL",
                    "Temperature:groupingSHRAY","Temperature:LLinf","groupingDEM:LLinf",
                    "groupingPEL:LLinf","groupingSHRAY:LLinf","Temperature:groupingDEM:LLinf",
                    "Temperature:groupingPEL:LLinf","Temperature:groupingSHRAY:LLinf","Rsquare")
  # parameter estimation of best model
  for (i in 1:5000){
    subFish <-  Resamp(fishes,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * grouping * LLinf +Tzero, data=subFish)
    M3[i,] <-c(coefficients(LM1),summary(LM1)$adj.r.squared)
  }

  # get the prediction from M3
  meanfish  <- colMeans(M3)
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","deep","dem","shray")

# calculate Q10 for asymptoic length 100 cm fish
  Te <- seq(0,30)
  Li <- rep(log10(100),length(Te))
  tz <- rep(0,length(Te))
  
  ypel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
           meanfish["Temperature:groupingPEL"]*Te+meanfish["groupingPEL:LLinf"]*Li+
           meanfish["Temperature:groupingPEL:LLinf"]*Te*Li
  Q10data[1,1] <- (10^(ypel[31])/10^(ypel[1]))^(10/(30-0))
  
  ydeep <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,1] <- (10^(ydeep[31])/10^(ydeep[1]))^(10/(30-0))
  
  ydem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
           meanfish["Temperature:groupingDEM"]*Te+meanfish["groupingDEM:LLinf"]*Li+
           meanfish["Temperature:groupingDEM:LLinf"]*Te*Li
  Q10data[3,1] <- (10^(ydem[31])/10^(ydem[1]))^(10/(30-0))
  
  yshray<- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
           meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
           meanfish["Temperature:groupingSHRAY"]*Te+meanfish["groupingSHRAY:LLinf"]*Li+
           meanfish["Temperature:groupingSHRAY:LLinf"]*Te*Li
  Q10data[4,1] <- (10^(yshray[31])/10^(yshray[1]))^(10/(30-0))
  
# calculate Q10 for asymptoic length 30 cm fish 
  Te <- seq(0,30)
  Li <- rep(log10(30),length(Te))
  tz <- rep(0,length(Te))
  
  yspel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingPEL"]*Te+meanfish["groupingPEL:LLinf"]*Li+
    meanfish["Temperature:groupingPEL:LLinf"]*Te*Li
  Q10data[1,2] <- (10^(yspel[31])/10^(yspel[1]))^(10/(30-0))
  
  ysdeep <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,2] <- (10^(ysdeep[31])/10^(ysdeep[1]))^(10/(30-0))
  
  ysdem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te+meanfish["groupingDEM:LLinf"]*Li+
    meanfish["Temperature:groupingDEM:LLinf"]*Te*Li
  Q10data[3,2] <- (10^(ysdem[31])/10^(ysdem[1]))^(10/(30-0))
  
  ysshray<- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te+meanfish["groupingSHRAY:LLinf"]*Li+
    meanfish["Temperature:groupingSHRAY:LLinf"]*Te*Li
  Q10data[4,2] <- (10^(ysshray[31])/10^(ysshray[1]))^(10/(30-0))
  
### unique species with highest growth
########################
  rm(list=ls())
  
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Processing_fish_data.R")
  
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Resample.R")
  
  library(data.table)
  fishes <- as.data.table(datFish)
  
  ### select highest growth rate per unique species
  fishes <- fishes[fishes[, .I[which.max(Arate)], by=Name]$V1]
  fishes <- as.data.frame(fishes)
  
  # derive ambient temperature
  fishes$Temperature <- -100
  for(i in 1:nrow(fishes)) {
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$Stemp[i] }    # temperature in first 100 meter 
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$Btemp[i]} # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
  }
  
  # group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" ] <- "PEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic" | fishes$Func_group == "Reef"] <- "DEM"
  
  # get log/log10 
  fishes$LLinf <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)

  # model selection
  LM1 <- lm(LArate ~ Temperature * grouping * LLinf +Tzero, data=fishes)
  LM2 <- lm(LArate ~ Temperature*grouping + grouping * LLinf +Tzero, data=fishes)
  LM3 <- lm(LArate ~ Temperature * LLinf + grouping*Temperature +Tzero, data=fishes)
  LM4 <- lm(LArate ~ LLinf * grouping + Temperature * LLinf +Tzero, data=fishes)
  LM5 <- lm(LArate ~ Temperature*grouping +  LLinf +Tzero, data=fishes)
  LM6 <- lm(LArate ~ Temperature * LLinf + grouping +Tzero, data=fishes)
  LM7 <- lm(LArate ~ LLinf * grouping + Temperature +Tzero, data=fishes)
  LM8 <- lm(LArate ~ LLinf + grouping + Temperature +Tzero, data=fishes)
  out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8)

  # get the prediction from M3
  meanfish  <- coefficients(LM3)
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","deep","dem","shray")
  
  # calculate Q10 for asymptoic length 100 cm fish
  Te <- seq(0,30)
  Li <- rep(log10(100),length(Te))
  tz <- rep(0,length(Te))
  
  ypel  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,1] <- (10^(ypel[31])/10^(ypel[1]))^(10/(30-0))
  
  ydeep  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,1] <- (10^(ydeep[31])/10^(ydeep[1]))^(10/(30-0))
  
  ydem  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,1] <- (10^(ydem[31])/10^(ydem[1]))^(10/(30-0))
  
  yshray  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,1] <- (10^(yshray[31])/10^(yshray[1]))^(10/(30-0))
  
  # calculate Q10 for asymptoic length 30 cm fish 
  Te <- seq(0,30)
  Li <- rep(log10(30),length(Te))
  tz <- rep(0,length(Te))
  
  yspel  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,2] <- (10^(yspel[31])/10^(yspel[1]))^(10/(30-0))
  
  ysdeep  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,2] <- (10^(ysdeep[31])/10^(ysdeep[1]))^(10/(30-0))
  
  ysdem  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,2] <- (10^(ysdem[31])/10^(ysdem[1]))^(10/(30-0))
  
  ysshray  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,2] <- (10^(ysshray[31])/10^(ysshray[1]))^(10/(30-0))
  

### resampling of observations and species
########################
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Processing_fish_data.R")
  
  setwd("H:/Werk/BP food web model/190319 - run for github/R-code processing")
  source("Resample.R")
  
  # use all observations
  fishes <- datFish
  
  # derive ambient temperature
  fishes$Temperature <- -100
  for(i in 1:nrow(fishes)) {
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$Stemp[i] }    # temperature in first 100 meter 
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$Btemp[i]} # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$Btemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$Btempdeep[i] } # bottom temperature deeper than 500 meters
  }
  
  # group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" ] <- "PEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic" | fishes$Func_group == "Reef"] <- "DEM"
  
  # get log10 
  fishes$LLinf <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)
  
  # model selection
  modFish <- matrix(data=NA,ncol=8,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8")
  
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    subFish <- subFish[sample(nrow(subFish), round(0.8*774)), ] # select subset species
    LM1 <- lm(LArate ~ Temperature * grouping * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*grouping + grouping * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + grouping*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * grouping + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*grouping +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + grouping +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * grouping + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + grouping + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8)
    modFish[i,] <- out$AIC
  }
  
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  # parameter estimation of best model
  M3 <- matrix(data=NA,ncol=12,nrow=5000)
  colnames(M3) <- c("Intercept","Temperature","LLinf","groupingDEM","groupingPEL","groupingSHRAY",
                    "Tzero","Temperature:LLinf","Temperature:groupingDEM","Temperature:groupingPEL",
                    "Temperature:groupingSHRAY","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(fishes,1) # select unique species
    subFish <- subFish[sample(nrow(subFish), round(0.8*774)), ] # select subset species
    LM3 <- lm(LArate ~ Temperature * LLinf + grouping*Temperature +Tzero, data=subFish)
    M3[i,] <-c(coefficients(LM3),summary(LM3)$adj.r.squared)
  }
  
  # get the prediction from M3
  meanfish  <- colMeans(M3)
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","deep","dem","shray")
  
  
  # calculate Q10 for asymptoic length 100 cm fish
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
  
  # calculate Q10 for asymptoic length 30 cm fish 
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