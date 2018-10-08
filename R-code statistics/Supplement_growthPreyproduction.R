rm(list=ls())

### FishBase growth data statistics with prey production instead of biomass (supplement)
####################################################
  setwd("..../R-code processing")
  source("Processing_fish_data.R")

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

### statistics per lifestyle: 
# We examined the effects of temperature and prey production 
# on growth using multiple linear regression.  
# The growth rate was log transformed (natural log). 
# Model selection was based on the Akaike Information Criterion (AIC) 
# (when models had a difference of 0-2 AIC units, we selected the model with the fewest parameters).
####################################################

# large pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  lpel <- subset(pel, pel$Linf > 80)
  mod1 <- lm(log(Arate)~Temperature + Zooprod, data=lpel)
  mod2 <- lm(log(Arate)~Temperature, data=lpel)
  mod3 <- lm(log(Arate)~Zooprod, data=lpel)
  AIC(mod1,mod2,mod3)
  Lpelmod <- mod3 # best model based on AIC
  Lpelmodtemp <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Lpelmodtemp)[1] + coef(Lpelmodtemp)[2]*x + coef(Lpelmodtemp)[3]*mean(lpel$Zooprod)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_Lpel<-y20/y10 

# small pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  spel <- subset(pel, pel$Linf <= 80)
  mod1 <- lm(log(Arate)~Temperature + Zooprod, data=spel)
  mod2 <- lm(log(Arate)~Temperature, data=spel)
  mod3 <- lm(log(Arate)~Zooprod, data=spel)
  AIC(mod1,mod2,mod3)
  spelmod <- mod2 # best model based on AIC

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(spelmod)[1] + coef(spelmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_spel<-y20/y10 

# shark and rays not included (no food data)

# deep-living fish not included (no food data)

# large demersal
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  ldem <- subset(dem, dem$Linf > 80)
  ldem <- ldem[complete.cases(ldem[ , "detrpod"]),]
  mod1 <- lm(log(Arate)~Temperature + detrpod, data=ldem)
  mod2 <- lm(log(Arate)~Temperature, data=ldem)
  mod3 <- lm(log(Arate)~detrpod, data=ldem)
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
  sdem <- sdem[complete.cases(sdem[ , "detrpod"]),]
  mod1 <- lm(log(Arate)~Temperature + detrpod, data=sdem)
  mod2 <- lm(log(Arate)~Temperature, data=sdem)
  mod3 <- lm(log(Arate)~detrpod, data=sdem)
  AIC(mod1,mod2,mod3)
  sdemmod <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(sdemmod)[1] + coef(sdemmod)[2]*x +coef(sdemmod)[3]*mean(sdem$detrpod)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_sdem<-y20/y10  

###### combine data into dataframe
  growth_groups <- data.frame(Guild = NA, obs = NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA,note=NA)
  growth_groups[1,] <- c("Large pelagics", nrow(lpel), as.character(Lpelmod$call)[1:2], coef(Lpelmod)[1], NA,coef(Lpelmod)[2],NA, summary(Lpelmod)$coefficients[2,4], Q10_Lpel,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  growth_groups[2,] <- c("Small pelagics", nrow(spel), as.character(spelmod$call)[1:2], coef(spelmod), NA, summary(spelmod)$coefficients[2,4],NA, Q10_spel,"-")  
  growth_groups[3,] <- c("Large demersal", nrow(ldem), as.character(ldemmod$call)[1:2], coef(ldemmod), NA, summary(ldemmod)$coefficients[2,4],NA, Q10_ldem,"-")  
  growth_groups[4,] <- c("Small demersal", nrow(sdem), as.character(sdemmod$call)[1:2], coef(sdemmod), summary(sdemmod)$coefficients[2:3,4], Q10_sdem,"-")  
  