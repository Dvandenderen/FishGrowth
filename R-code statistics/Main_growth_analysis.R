rm(list=ls())

### FishBase growth data statistics, main analysis
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
  # We examined the effects of temperature and prey biomass 
  # on growth using multiple linear regression. Since prey 
  # biomass is only available for pelagic and demersal fish, 
  # we analyzed growth in each ecological lifestyle separately. 
  # The growth rate was log transformed. 
  # Model selection was based on the Akaike Information Criterion (AIC) 
  # (when models had a difference of 0-2 AIC units, we selected the model with the fewest parameters).
####################################################
  
# large pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  lpel <- subset(pel, pel$Linf > 80)
  mod1 <- lm(log(Arate)~Temperature + Zoobio, data=lpel)
  mod2 <- lm(log(Arate)~Temperature, data=lpel)
  mod3 <- lm(log(Arate)~Zoobio, data=lpel)
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
  mod1 <- lm(log(Arate)~Temperature + Zoobio, data=spel)
  mod2 <- lm(log(Arate)~Temperature, data=spel)
  mod3 <- lm(log(Arate)~Zoobio, data=spel)
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
  shraymodel <- lm(log(Arate)~Temperature, data=shray)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(shraymodel)[1] + coef(shraymodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_shray<-y20/y10  
  
# deep-living fish
  deep <- subset(fishes, fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal")
  deepmodel <- lm(log(Arate)~Temperature, data=deep)
  
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
  mod1 <- lm(log(Arate)~Temperature + Bentbio, data=ldem)
  mod2 <- lm(log(Arate)~Temperature, data=ldem)
  mod3 <- lm(log(Arate)~Bentbio, data=ldem)
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
  mod1 <- lm(log(Arate)~Temperature + Bentbio, data=sdem)
  mod2 <- lm(log(Arate)~Temperature, data=sdem)
  mod3 <- lm(log(Arate)~Bentbio, data=sdem)
  AIC(mod1,mod2,mod3)
  sdemmod <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(sdemmod)[1] + coef(sdemmod)[2]*x +coef(sdemmod)[3]*mean(sdem$Bentbio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_sdem<-y20/y10  
  
###### combine data into dataframe
  growth_groups <- data.frame(Guild = NA, obs = NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA,note=NA)
  growth_groups[1,] <- c("Large pelagics", nrow(lpel), as.character(Lpelmod$call)[1:2], coef(Lpelmod), NA, summary(Lpelmod)$coefficients[2,4],NA, Q10_Lpel,"-")  
  growth_groups[2,] <- c("Small pelagics", nrow(spel), as.character(spelmod$call)[1:2], coef(spelmod), NA, summary(spelmod)$coefficients[2,4],NA, Q10_spel,"-")  
  growth_groups[3,] <- c("Sharks and rays", nrow(shray), as.character(shraymodel$call)[1:2], coef(shraymodel), NA, summary(shraymodel)$coefficients[2,4],NA, Q10_shray,"-")  
  growth_groups[4,] <- c("Large demersal", nrow(ldem), as.character(ldemmod$call)[1:2], coef(ldemmod), NA, summary(ldemmod)$coefficients[2,4],NA, Q10_ldem,"-")  
  growth_groups[5,] <- c("Small demersal", nrow(sdem), as.character(sdemmod$call)[1:2], coef(sdemmod), summary(sdemmod)$coefficients[2:3,4], Q10_sdem,"-")  
  growth_groups[6,] <- c("Deep-living fish",nrow(deep), as.character(deepmodel$call)[1:2], coef(deepmodel), NA, summary(deepmodel)$coefficients[2,4],NA, Q10_deep,"-")  

### statistics per sub-lifestyle for small demersals: 
# We examined the effects of temperature and prey biomass 
# on growth using multiple linear regression. The growth rate 
# was log10 transformed to improve model fit. 
# Model selection was based on the Akaike Information Criterion (AIC) 
# (when models had a difference of 0-2 AIC units, we selected the model with the fewest parameters).
####################################################

# select small demersals
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  dem <- subset(fishes, fishes$Linf <= 80)
  dem <- dem[complete.cases(dem[ , "Bentbio"]),]
  
# reef-asscociated
  reef<- subset(dem,dem$Func_group =="Reef")
  mod1 <- lm(log(Arate)~Temperature + reef$Bentbio, data=reef)
  mod2 <- lm(log(Arate)~Temperature, data=reef)
  mod3 <- lm(log(Arate)~Bentbio, data=reef)
  AIC(mod1,mod2,mod3)
  reefmod <- mod2
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(reefmod)[1] + coef(reefmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_reef<-y20/y10  
  
# benthopelagic
  BenPo<- subset(dem,dem$Func_group =="Benthopelagic")
  mod1 <- lm(log(Arate)~Temperature + Bentbio, data=BenPo)
  mod2 <- lm(log(Arate)~Temperature, data=BenPo)
  mod3 <- lm(log(Arate)~Bentbio, data=BenPo)
  AIC(mod1,mod2,mod3)
  benpomod <- mod3
  benpomod_temp <- mod1 ## best model without temperature Q10 is calculated from the model: Growth ~ Temp + Prey 
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(benpomod_temp)[1] + coef(benpomod_temp)[2]*x + coef(benpomod_temp)[3]*mean(BenPo$Bentbio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_benpo<-y20/y10  
  
# bottom-dwellers
  Benthic<- subset(dem,dem$Func_group =="Demersal")
  mod1 <- lm(log(Arate)~Temperature + Bentbio, data=Benthic)
  mod2 <- lm(log(Arate)~Temperature, data=Benthic)
  mod3 <- lm(log(Arate)~Bentbio, data=Benthic)
  AIC(mod1,mod2,mod3)
  botmod <- mod1
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(botmod)[1] + coef(botmod)[2]*x + coef(botmod)[3]*mean(Benthic$Bentbio)
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_bot<-y20/y10  

# Flatfishes
  Flat<- subset(dem,dem$Func_group =="Flatfish")
  mod1 <- lm(log(Arate)~Temperature + Bentbio, data=Flat)
  mod2 <- lm(log(Arate)~Temperature, data=Flat)
  mod3 <- lm(log(Arate)~Bentbio, data=Flat)
  AIC(mod1,mod2,mod3)
  flatmod <- mod2
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(flatmod)[1] + coef(flatmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_flat<-y20/y10  

###### combine data into dataframe
  growth_sub <- data.frame(Guild = NA, obs =NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA,note=NA)
  growth_sub[1,] <- c("Reef-associated", nrow(reef), as.character(reefmod$call)[1:2], coef(reefmod), NA, summary(reefmod)$coefficients[2,4],NA, Q10_reef,"-")  
  growth_sub[2,] <- c("Benthopelagics", nrow(BenPo),as.character(benpomod$call)[1:2], coef(benpomod)[1], NA,coef(benpomod)[2], NA, summary(benpomod)$coefficients[2,4], Q10_benpo,"Q10 is calculated from the model: Linf ~ Temp + Prey. Inclusion of temperature does not improve model fit, the temperature effect is calculated for average food concentrations")  
  growth_sub[3,] <- c("Bottom-dwellers", nrow(Benthic), as.character(botmod$call)[1:2], coef(botmod), summary(botmod)$coefficients[2:3,4], Q10_bot,"-")  
  growth_sub[4,] <- c("Flatfishes",nrow(Flat), as.character(flatmod$call)[1:2], coef(flatmod), NA, summary(flatmod)$coefficients[2,4],NA, Q10_flat,"-")  
  
  
