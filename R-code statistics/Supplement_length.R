
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
  fishes$LLinf <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)

  # model selection
  modFish <- matrix(data=NA,ncol=5,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LLinf ~ Temperature * grouping, data=subFish)
    LM2 <- lm(LLinf ~ Temperature + grouping, data=subFish)
    LM3 <- lm(LLinf ~ Temperature, data=subFish)
    LM4 <- lm(LLinf ~ grouping, data=subFish)
    LM5 <- lm(LLinf ~ 1, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  # LM4 is most supported model hence no temperature effect

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
  
  # get log10 
  fishes$LLinf <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)

  # model selection
  LM1 <- lm(LLinf ~ Temperature * grouping, data=fishes)
  LM2 <- lm(LLinf ~ Temperature + grouping, data=fishes)
  LM3 <- lm(LLinf ~ Temperature, data=fishes)
  LM4 <- lm(LLinf ~ grouping, data=fishes)
  LM5 <- lm(LLinf ~ 1, data=fishes)
  out <- AIC(LM1,LM2,LM3,LM4,LM5)
  
  # LM1 is most supported model (but very close with LM4)
  
  # get the prediction
  meanfish  <- coefficients(LM1)
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =1, nrow =4)
  rownames(Q10data) <- c("pel","deep","dem","shray")
  
  # calculate Q10 for asymptoic length 100 cm fish
  Te <- seq(0,30)

  ypel  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+ meanfish["groupingPEL"]+
           meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,1] <- (10^(ypel[31])/10^(ypel[1]))^(10/(30-0))
  
  ydeep  <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te
  Q10data[2,1] <- (10^(ydeep[31])/10^(ydeep[1]))^(10/(30-0))
  
  ydem <- meanfish["(Intercept)"]+meanfish["Temperature"]*Te+ meanfish["groupingDEM"]+
          meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,1] <- (10^(ydem[31])/10^(ydem[1]))^(10/(30-0))
  
  yshray  <-  meanfish["(Intercept)"]+meanfish["Temperature"]*Te+ meanfish["groupingSHRAY"]+
              meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,1] <- (10^(yshray[31])/10^(yshray[1]))^(10/(30-0))
  
  
### resampling of observations and species
########################
  rm(list=ls())
  
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
  modFish <- matrix(data=NA,ncol=5,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    subFish <- subFish[sample(nrow(subFish), round(0.8*774)), ] # select subset species
    LM1 <- lm(LLinf ~ Temperature * grouping, data=subFish)
    LM2 <- lm(LLinf ~ Temperature + grouping, data=subFish)
    LM3 <- lm(LLinf ~ Temperature, data=subFish)
    LM4 <- lm(LLinf ~ grouping, data=subFish)
    LM5 <- lm(LLinf ~ 1, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  # LM4 is most supported model (but close with LM1) hence no temperature effect
 