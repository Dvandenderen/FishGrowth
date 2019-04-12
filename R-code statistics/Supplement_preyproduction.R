
### supplement to analyze robustness of patterns with prey production instead of biomass
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  source("Processing_fish_data.R")
  
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
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
  
  #### M5_sub zooplankton production
  sPel <-   subset(fishes,fishes$grouping =="PEL" & fishes$Linf < 50)
  
  # select best model
  modFish <- matrix(data=NA,ncol=14,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8","LM9","LM10","LM11","LM12","LM13","LM14")
  
  for (i in 1:5000){
    subFish <- Resamp(sPel,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * Zooprod * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*Zooprod + Zooprod * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + Zooprod*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * Zooprod + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*Zooprod +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + Zooprod +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * Zooprod + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + Zooprod + Temperature +Tzero, data=subFish)
    LM9 <- lm(LArate ~ LLinf * Zooprod +Tzero, data=subFish)
    LM10<- lm(LArate ~ LLinf + Zooprod +Tzero, data=subFish)
    LM11<- lm(LArate ~ LLinf * Temperature +Tzero, data=subFish)
    LM12<- lm(LArate ~ LLinf + Temperature +Tzero, data=subFish)
    LM13<- lm(LArate ~ Zooprod * Temperature +Tzero, data=subFish)
    LM14<- lm(LArate ~ Zooprod + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8,LM9,LM10,LM11,LM12,LM13,LM14)
    modFish[i,] <- out$AIC
  }
  
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  M5 <- matrix(data=NA,ncol=6,nrow=5000)
  colnames(M5) <- c("Intercept","LLinf","Temperature","Tzero","LLinf:Temperature","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(sPel,1) # select unique species
    LM11<- lm(LArate ~  LLinf * Temperature +Tzero, data=subFish)
    M5[i,] <-c(coefficients(LM11),summary(LM11)$adj.r.squared)
  }  
  
  colMeans(M5)
  
  #### M6_sub detrital flux to the seabed 
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Bentbio)))
  length(unique(sDem$Name))
  
  # select best model
  modFish <- matrix(data=NA,ncol=14,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8","LM9","LM10","LM11","LM12","LM13","LM14")
  
  for (i in 1:5000){
    subFish <- Resamp(sDem,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * detrpod * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*detrpod + detrpod * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + detrpod*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * detrpod + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*detrpod +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + detrpod +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * detrpod + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + detrpod + Temperature +Tzero, data=subFish)
    LM9 <- lm(LArate ~ LLinf * detrpod +Tzero, data=subFish)
    LM10<- lm(LArate ~ LLinf + detrpod +Tzero, data=subFish)
    LM11<- lm(LArate ~ LLinf * Temperature +Tzero, data=subFish)
    LM12<- lm(LArate ~ LLinf + Temperature +Tzero, data=subFish)
    LM13<- lm(LArate ~ detrpod * Temperature +Tzero, data=subFish)
    LM14<- lm(LArate ~ detrpod + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8,LM9,LM10,LM11,LM12,LM13,LM14)
    modFish[i,] <- out$AIC
  }
  
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
  M6 <- matrix(data=NA,ncol=6,nrow=5000)
  colnames(M6) <- c("Intercept","LLinf","detrpod","Temperature","Tzero","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(sDem,1) # select unique species
    LM8 <- lm(LArate ~ LLinf + detrpod + Temperature +Tzero, data=subFish)
    M6[i,] <-c(coefficients(LM8),summary(LM8)$adj.r.squared)
  }  
  
  colMeans(M6)
  
  