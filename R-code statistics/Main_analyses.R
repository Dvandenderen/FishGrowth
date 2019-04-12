
### main analysis
####################################################
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
  fishes$LLinf  <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$Arate)
  
# M1 (number corresponds to table 1, main manuscript)
####################################################
  
# model selection
  modFish <- matrix(data=NA,ncol=2,nrow=5000)
  colnames(modFish) <- c("LM1","LM2")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LArate ~ 1 , data=subFish)
    LM2 <- lm(LArate ~ Temperature , data=subFish)
    out <- AIC(LM1,LM2)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
# parameter estimation of best model
  M1 <- matrix(data=NA,ncol=3,nrow=5000)
  colnames(M1) <- c("Intercept","Temperature","Rsquare")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LArate ~ Temperature , data=subFish)
    M1[i,] <-c(coefficients(LM1),summary(LM1)$adj.r.squared)
  }
 
#### M2 (number corresponds to table 1, main manuscript)  
####################################################
  
# model selection
  modFish <- matrix(data=NA,ncol=2,nrow=5000)
  colnames(modFish) <- c("LM1","LM2")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LLinf ~ 1 , data=subFish)
    LM2 <- lm(LLinf ~ Temperature , data=subFish)
    out <- AIC(LM1,LM2)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
# parameter estimation of best model
  M2 <- matrix(data=NA,ncol=2,nrow=5000)
  colnames(M2) <- c("Intercept","Rsquare")
  for (i in 1:5000){
    subFish <- Resamp(fishes,1) # select unique species
    LM1 <- lm(LLinf ~ 1  , data=subFish)
    M2[i,] <-c(coefficients(LM1),summary(LM1)$adj.r.squared)
  }
  
#### M3 (number corresponds to table 1, main manuscript)  
####################################################
  
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

# parameter estimation of best model
  M3 <- matrix(data=NA,ncol=12,nrow=5000)
  colnames(M3) <- c("Intercept","Temperature","LLinf","groupingDEM","groupingPEL","groupingSHRAY",
                         "Tzero","Temperature:LLinf","Temperature:groupingDEM","Temperature:groupingPEL",
                         "Temperature:groupingSHRAY","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(fishes,1) # select unique species
    LM3 <- lm(LArate ~ Temperature * LLinf + grouping*Temperature +Tzero, data=subFish)
    M3[i,] <-c(coefficients(LM3),summary(LM3)$adj.r.squared)
  }
  
#### M4 (number corresponds to table 1, main manuscript)  
####################################################
  
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
  
# parameter estimation of best model
  M4 <- matrix(data=NA,ncol=9,nrow=5000)
  colnames(M4) <- c("Intercept","Temperature","groupingDEM","groupingPEL","groupingSHRAY",
                    "Temperature:groupingDEM","Temperature:groupingPEL",
                    "Temperature:groupingSHRAY","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(fishes,1) # select unique species
    LM1 <- lm(LLinf ~ Temperature * grouping, data=subFish)
    M4[i,] <-c(coefficients(LM1),summary(LM1)$adj.r.squared)
  }
  
#### M5 (number corresponds to table 1, main manuscript)  
####################################################
  sPel <-   subset(fishes,fishes$grouping =="PEL" & fishes$Linf < 50)
  
# model selection
  modFish <- matrix(data=NA,ncol=14,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8","LM9",
                         "LM10","LM11","LM12","LM13","LM14")
  for (i in 1:5000){
    subFish <- Resamp(sPel,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * Zoobio * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*Zoobio + Zoobio * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + Zoobio*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * Zoobio + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*Zoobio +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + Zoobio +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * Zoobio + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + Zoobio + Temperature +Tzero, data=subFish)
    LM9 <- lm(LArate ~ LLinf * Zoobio +Tzero, data=subFish)
    LM10<- lm(LArate ~ LLinf + Zoobio +Tzero, data=subFish)
    LM11<- lm(LArate ~ LLinf * Temperature +Tzero, data=subFish)
    LM12<- lm(LArate ~ LLinf + Temperature +Tzero, data=subFish)
    LM13<- lm(LArate ~ Zoobio * Temperature +Tzero, data=subFish)
    LM14<- lm(LArate ~ Zoobio + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8,LM9,LM10,LM11,LM12,LM13,LM14)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)

# parameter estimation of best model  
  M5 <- matrix(data=NA,ncol=6,nrow=5000)
  colnames(M5) <- c("Intercept","LLinf","Temperature","Tzero","LLinf:Temperature","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(sPel,1) # select unique species
    LM11<- lm(LArate ~  LLinf * Temperature +Tzero, data=subFish)
    M5[i,] <-c(coefficients(LM11),summary(LM11)$adj.r.squared)
  }  
  
#### M6 (number corresponds to table 1, main manuscript)  
####################################################
  sDem <-   subset(fishes,fishes$grouping =="DEM" & fishes$Linf < 50)
  sDem <- subset(sDem,!(is.na(sDem$Bentbio)))
  length(unique(sDem$Name))
    
# model selection
  modFish <- matrix(data=NA,ncol=14,nrow=5000)
  colnames(modFish) <- c("LM1","LM2","LM3","LM4","LM5","LM6","LM7","LM8",
                         "LM9","LM10","LM11","LM12","LM13","LM14")
  for (i in 1:5000){
    subFish <- Resamp(sDem,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * Bentbio * LLinf +Tzero, data=subFish)
    LM2 <- lm(LArate ~ Temperature*Bentbio + Bentbio * LLinf +Tzero, data=subFish)
    LM3 <- lm(LArate ~ Temperature * LLinf + Bentbio*Temperature +Tzero, data=subFish)
    LM4 <- lm(LArate ~ LLinf * Bentbio + Temperature * LLinf +Tzero, data=subFish)
    LM5 <- lm(LArate ~ Temperature*Bentbio +  LLinf +Tzero, data=subFish)
    LM6 <- lm(LArate ~ Temperature * LLinf + Bentbio +Tzero, data=subFish)
    LM7 <- lm(LArate ~ LLinf * Bentbio + Temperature +Tzero, data=subFish)
    LM8 <- lm(LArate ~ LLinf + Bentbio + Temperature +Tzero, data=subFish)
    LM9 <- lm(LArate ~ LLinf * Bentbio +Tzero, data=subFish)
    LM10<- lm(LArate ~ LLinf + Bentbio +Tzero, data=subFish)
    LM11<- lm(LArate ~ LLinf * Temperature +Tzero, data=subFish)
    LM12<- lm(LArate ~ LLinf + Temperature +Tzero, data=subFish)
    LM13<- lm(LArate ~ Bentbio * Temperature +Tzero, data=subFish)
    LM14<- lm(LArate ~ Bentbio + Temperature +Tzero, data=subFish)
    out <- AIC(LM1,LM2,LM3,LM4,LM5,LM6,LM7,LM8,LM9,LM10,LM11,LM12,LM13,LM14)
    modFish[i,] <- out$AIC
  }
  modsupport <- as.matrix(apply( modFish, 1, which.min))
  table(modsupport)
  
# parameter estimation of best model
  M6 <- matrix(data=NA,ncol=10,nrow=5000)
  colnames(M6) <- c("Intercept","Temperature","Bentbio","LLinf","Tzero",
                    "Temperature:Bentbio","Temperature:LLinf",
                    "Bentbio:LLinf","Temperature:Bentbio:LLinf","Rsquare")
  for (i in 1:5000){
    subFish <-  Resamp(sDem,1) # select unique species
    LM1 <- lm(LArate ~ Temperature * Bentbio * LLinf +Tzero, data=subFish)
    M6[i,] <-c(coefficients(LM1),summary(LM1)$adj.r.squared)
  }  
  
  rm(list= ls()[!(ls() %in% c('M1','M2','M3','M4','M5','M6','fishes'))])
  
  sumdata <- list(M1,M2,M3,M4,M5,M6,fishes)
  save(sumdata,file="Processed_files.Rdata")  
  
  