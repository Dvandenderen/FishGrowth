rm(list=ls())

### FishBase asymptotic size data statistics, supplementary 
####################################################

### analysis t0 < +-1
####################################################
  setwd("..../R-code processing")
  source("Processing_fish_data.R")
  
  datFish<-subset(datFish,datFish$Tzero <= 1 & datFish$Tzero >= -1)

# aggregate species by unique region 
  library(dplyr)
  fishes<-datFish %>% 
    group_by(Name,uniReg) %>%
    summarise_each (funs(mean(., na.rm = TRUE)), Arate,K,Linf,Zoobio,Zooprod,Stemp,Btemp,Btempdeep,
                    Bentbio,Benthbiodeep,detrpod,detrpoddeep)
  fishes <- as.data.frame(fishes)

# couple to functional group (again)
  gro<-subset(datFish, !duplicated(datFish$Name))
  fishes<-cbind(fishes,gro[match(fishes$Name,gro$Name),c(10)])
  colnames(fishes)[15]<-"Func_group"

# calculate ambient temperature
  fishes$Temperature <- -100
  
  for(i in 1:nrow(fishes)) {
    ### temperature in first 100 meter 
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$Stemp[i] }
    
    ### bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$Btemp[i]}
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    
    ### bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$Btempdeep[i] }
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$Btempdeep[i] }
  }

# calculate temperature as 1/cT with c = Boltzmann constant and T in kelvin
  fishes$Temperature = 1/((8.62*10^-5)*(fishes$Temperature+273.2))

# large pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  lpel <- subset(pel, pel$Linf > 80)
  mod1 <- lm(log(Linf)~Temperature + Zoobio, data=lpel)
  mod2 <- lm(log(Linf)~Temperature, data=lpel)
  mod3 <- lm(log(Linf)~Zoobio, data=lpel)
  AIC(mod1,mod2,mod3)
  Lpelmod <- mod3 # best model based on AIC
  Lpelmodtemp <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Lpelmodtemp)[1] + coef(Lpelmodtemp)[2]*x  + coef(Lpelmodtemp)[3]*mean(lpel$Zoobio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_Lpel<-y20/y10 
  
# small pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  spel <- subset(pel, pel$Linf <= 80)
  mod1 <- lm(log(Linf)~Temperature + Zoobio, data=spel)
  mod2 <- lm(log(Linf)~Temperature, data=spel)
  mod3 <- lm(log(Linf)~Zoobio, data=spel)
  AIC(mod1,mod2,mod3)
  spelmod <- mod3 # best model based on AIC
  spelmodtemp <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(spelmodtemp)[1] + coef(spelmodtemp)[2]*x + coef(spelmodtemp)[3]*mean(spel$Zoobio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_spel<-y20/y10 
  
# shark and rays (no food data)
  shray <- subset(fishes, fishes$Func_group == "Shark" | fishes$Func_group == "Ray")
  shraymodel <- lm(log(Linf)~Temperature, data=shray)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(shraymodel)[1] + coef(shraymodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_shray<-y20/y10  
  
# deep-living fish
  deep <- subset(fishes, fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal")
  deepmodel <- lm(log(Linf)~Temperature, data=deep)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(deepmodel)[1] + coef(deepmodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_deep<-y20/y10  
  
# large demersal
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  ldem <- subset(dem, dem$Linf > 80)
  ldem <- ldem[complete.cases(ldem[ , "Bentbio"]),]
  mod1 <- lm(log(Linf)~Temperature + Bentbio, data=ldem)
  mod2 <- lm(log(Linf)~Temperature, data=ldem)
  mod3 <- lm(log(Linf)~Bentbio, data=ldem)
  AIC(mod1,mod2, mod3)
  ldemmod <- mod2
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(ldemmod)[1] + coef(ldemmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_ldem<-y20/y10  
  
# small demersal 
  sdem <- subset(dem, dem$Linf <= 80)
  sdem <- sdem[complete.cases(sdem[ , "Bentbio"]),]
  mod1 <- lm(log(Linf)~Temperature + Bentbio, data=sdem)
  mod2 <- lm(log(Linf)~Temperature, data=sdem)
  mod3 <- lm(log(Linf)~Bentbio, data=sdem)
  AIC(mod1,mod2,mod3)
  sdemmod <- mod3
  sdemmodtemp <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(sdemmodtemp)[1] + coef(sdemmodtemp)[2]*x +coef(sdemmodtemp)[3]*mean(sdem$Bentbio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_sdem<-y20/y10  
  
  ###### combine data into dataframe
  Linf_groups_Tzeroone <- data.frame(Guild = NA, obs= NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA, note = NA)
  Linf_groups_Tzeroone[1,] <- c("Large pelagics", nrow(lpel), as.character(Lpelmod$call)[1:2], coef(Lpelmod)[1], NA, coef(Lpelmod)[2],NA, summary(Lpelmod)$coefficients[2,4], Q10_Lpel,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  Linf_groups_Tzeroone[2,] <- c("Small pelagics", nrow(spel), as.character(spelmod$call)[1:2], coef(spelmod)[1], NA, coef(spelmod)[2],NA, summary(spelmod)$coefficients[2,4], Q10_spel,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  Linf_groups_Tzeroone[3,] <- c("Sharks and rays", nrow(shray), as.character(shraymodel$call)[1:2], coef(shraymodel), NA, summary(shraymodel)$coefficients[2,4],NA, Q10_shray,"-")  
  Linf_groups_Tzeroone[4,] <- c("Large demersal", nrow(ldem), as.character(ldemmod$call)[1:2], coef(ldemmod), NA, summary(ldemmod)$coefficients[2,4],NA, Q10_ldem,"-")  
  Linf_groups_Tzeroone[5,] <- c("Small demersal", nrow(sdem), as.character(sdemmod$call)[1:2], coef(sdemmod)[1],NA, coef(sdemmod)[2],NA, summary(sdemmod)$coefficients[2,4], Q10_sdem,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  Linf_groups_Tzeroone[6,] <- c("Deep-living fish", nrow(deep), as.character(deepmodel$call)[1:2], coef(deepmodel), NA, summary(deepmodel)$coefficients[2,4],NA, Q10_deep,"-")  
  
### analysis unique species (highest growth rate selected)
####################################################
rm(list=ls())
  setwd("..../R-code processing")
  source("Processing_fish_data.R")
  
# aggregate species by unique region 
  library(dplyr)
  fishes<-datFish %>% 
    group_by(Name,uniReg) %>%
    summarise_each (funs(mean(., na.rm = TRUE)), Arate,K,Linf,Zoobio,Zooprod,Stemp,Btemp,Btempdeep,
                    Bentbio,Benthbiodeep,detrpod,detrpoddeep)
  fishes <- as.data.frame(fishes)
  
### select when aggregating per unique species
  fishes2<-fishes %>% 
    group_by(Name) %>%
    summarise_each (funs(max(., na.rm = TRUE)), Arate)
  fishes$unic <- paste(fishes$Name,fishes$Arate)
  fishes2$unic <- paste(fishes2$Name,fishes2$Arate)
  fishes<- cbind(fishes,fishes2[match(fishes$unic,fishes2$unic),c(2)])
  colnames(fishes)[16] <- "newArate"
  fishes <- subset(fishes,!(is.na(fishes$newArate)))
  
# couple to functional group (again)
  gro<-subset(datFish, !duplicated(datFish$Name))
  fishes<-cbind(fishes,gro[match(fishes$Name,gro$Name),c(10)])
  colnames(fishes)[17]<-"Func_group"
  
# calculate ambient temperature
  fishes$Temperature <- -100
  
  for(i in 1:nrow(fishes)) {
    ### temperature in first 100 meter 
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$Stemp[i] }
    
    ### bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$Btemp[i]}
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$Btemp[i] }
    
    ### bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$Btempdeep[i] }
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$Btempdeep[i] }
  }
  
# calculate temperature as 1/cT with c = Boltzmann constant and T in kelvin
  fishes$Temperature = 1/((8.62*10^-5)*(fishes$Temperature+273.2))
  
# large pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  lpel <- subset(pel, pel$Linf > 80)
  mod1 <- lm(log(Linf)~Temperature + Zoobio, data=lpel)
  mod2 <- lm(log(Linf)~Temperature, data=lpel)
  mod3 <- lm(log(Linf)~Zoobio, data=lpel)
  AIC(mod1,mod2,mod3)
  Lpelmod <- mod2 # best model based on AIC

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*x 
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_Lpel<-y20/y10 
  
# small pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  spel <- subset(pel, pel$Linf <= 80)
  mod1 <- lm(log(Linf)~Temperature + Zoobio, data=spel)
  mod2 <- lm(log(Linf)~Temperature, data=spel)
  mod3 <- lm(log(Linf)~Zoobio, data=spel)
  AIC(mod1,mod2,mod3)
  spelmod <- mod2 # best model based on AIC

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(spelmod)[1] + coef(spelmod)[2]*x 
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_spel<-y20/y10 
  
# shark and rays (no food data)
  shray <- subset(fishes, fishes$Func_group == "Shark" | fishes$Func_group == "Ray")
  shraymodel <- lm(log(Linf)~Temperature, data=shray)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(shraymodel)[1] + coef(shraymodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_shray<-y20/y10  
  
# deep-living fish
  deep <- subset(fishes, fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal")
  deepmodel <- lm(log(Linf)~Temperature, data=deep)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(deepmodel)[1] + coef(deepmodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_deep<-y20/y10  
  
# large demersal
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  ldem <- subset(dem, dem$Linf > 80)
  ldem <- ldem[complete.cases(ldem[ , "Bentbio"]),]
  mod1 <- lm(log(Linf)~Temperature + Bentbio, data=ldem)
  mod2 <- lm(log(Linf)~Temperature, data=ldem)
  mod3 <- lm(log(Linf)~Bentbio, data=ldem)
  AIC(mod1,mod2, mod3)
  ldemmod <- mod2
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(ldemmod)[1] + coef(ldemmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_ldem<-y20/y10  
  
# small demersal 
  sdem <- subset(dem, dem$Linf <= 80)
  sdem <- sdem[complete.cases(sdem[ , "Bentbio"]),]
  mod1 <- lm(log(Linf)~Temperature + Bentbio, data=sdem)
  mod2 <- lm(log(Linf)~Temperature, data=sdem)
  mod3 <- lm(log(Linf)~Bentbio, data=sdem)
  AIC(mod1,mod2,mod3)
  sdemmod <- mod3
  sdemmodtemp <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(sdemmodtemp)[1] + coef(sdemmodtemp)[2]*x +coef(sdemmodtemp)[3]*mean(sdem$Bentbio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_sdem<-y20/y10  
  
###### combine data into dataframe
  Linf_groups_uniq <- data.frame(Guild = NA, obs= NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA, note = NA)
  Linf_groups_uniq[1,] <- c("Large pelagics", nrow(lpel), as.character(Lpelmod$call)[1:2], coef(Lpelmod), NA, summary(Lpelmod)$coefficients[2,4],NA, Q10_Lpel,"-")  
  Linf_groups_uniq[2,] <- c("Small pelagics", nrow(spel), as.character(spelmod$call)[1:2], coef(spelmod), NA, summary(spelmod)$coefficients[2,4],NA, Q10_spel,"-")  
  Linf_groups_uniq[3,] <- c("Sharks and rays", nrow(shray), as.character(shraymodel$call)[1:2], coef(shraymodel), NA, summary(shraymodel)$coefficients[2,4],NA, Q10_shray,"-")  
  Linf_groups_uniq[4,] <- c("Large demersal", nrow(ldem), as.character(ldemmod$call)[1:2], coef(ldemmod), NA, summary(ldemmod)$coefficients[2,4],NA, Q10_ldem,"-")  
  Linf_groups_uniq[5,] <- c("Small demersal", nrow(sdem), as.character(sdemmod$call)[1:2], coef(sdemmod)[1],NA, coef(sdemmod)[2],NA, summary(sdemmod)$coefficients[2,4], Q10_sdem,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  Linf_groups_uniq[6,] <- c("Deep-living fish", nrow(deep), as.character(deepmodel$call)[1:2], coef(deepmodel), NA, summary(deepmodel)$coefficients[2,4],NA, Q10_deep,"-")  
  