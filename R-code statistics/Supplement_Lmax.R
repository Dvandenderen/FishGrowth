  rm(list=ls())

### FishBase maximum size (Lmax), supplementary 
####################################################

####################################################
  setwd("..../R-code processing")
  source("Processing_fish_data.R")

### get unique species and use FishBase for Lmax
####################################################
  library(rfishbase)
  
  uni_spec <- unique(datFish$Name)
  uniFish <- species(paste(uni_spec)) ## takes 10 minutes
  
  datFish <- cbind(datFish,uniFish[match(datFish$Name,uniFish$sciname),c("Length")])
  colnames(datFish)[21] <- "Lmax"

# aggregate species by mean temperature and Lmax 
  library(dplyr)
  fishes<-datFish %>% 
    group_by(Name) %>%
    summarise_each (funs(mean(., na.rm = TRUE)),Stemp,Btemp,Btempdeep,Lmax)
  fishes <- as.data.frame(fishes)

# couple to functional group (again)
  gro<-subset(datFish, !duplicated(datFish$Name))
  fishes<-cbind(fishes,gro[match(fishes$Name,gro$Name),c(10)])
  colnames(fishes)[6]<-"Func_group"

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

  fishes <- subset(fishes,!(is.na(fishes$Lmax)))
  
# large pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  lpel <- subset(pel, pel$Lmax > 80)
  Lpelmod <- lm(log(Lmax)~Temperature, data=lpel)

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(Lpelmod)[1] + coef(Lpelmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_Lpel <-y20/y10 
  
# small pelagic
  pel <- subset(fishes, fishes$Func_group == "Pelagic") 
  spel <- subset(pel, pel$Lmax <= 80)
  spelmod <- lm(log(Lmax)~Temperature, data=spel)

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(spelmod)[1] + coef(spelmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_spel <-y20/y10 
  
# sharks and rays
  shray <- subset(fishes, fishes$Func_group == "Shark" | fishes$Func_group == "Ray")
  shraymodel <- lm(log(Lmax)~Temperature, data=shray)

  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(shraymodel)[1] + coef(shraymodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_shray <-y20/y10 
  
# large demersals 
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  ldem <- subset(dem, dem$Lmax > 80)
  ldemmod <- lm(log(Lmax)~Temperature, data=ldem)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(ldemmod)[1] + coef(ldemmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_ldem <-y20/y10 
  
# small demersals 
  dem <- subset(fishes, fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef")
  sdem <- subset(dem, dem$Lmax <= 80)
  sdemmod <- lm(log(Lmax)~Temperature, data=sdem)
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(sdemmod)[1] + coef(sdemmod)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_sdem <-y20/y10 
  
# deep-living fish
  deep <- subset(fishes, fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal")
  deepmodel <- lm(log(Lmax)~Temperature, data=deep) 
  
  x <- c(10,20)
  x <- 1/((8.62*10^-5)*(x+273.2))
  y<- coef(deepmodel)[1] + coef(deepmodel)[2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10_deep <-y20/y10 
  
  ###### combine data into dataframe
  growth_groups <- data.frame(Guild = NA, obs = NA, Model = NA, BestMod = NA, Intercept = NA, Temperature=NA, Preybio = NA, Pvalue_temp = NA, Pvalue_food=NA, Q10 = NA,note=NA)
  growth_groups[1,] <- c("Large pelagics", nrow(lpel), as.character(Lpelmod$call)[1:2], coef(Lpelmod), NA, summary(Lpelmod)$coefficients[2,4],NA, Q10_Lpel,"-")  
  growth_groups[2,] <- c("Small pelagics", nrow(spel), as.character(spelmod$call)[1:2], coef(spelmod), NA, summary(spelmod)$coefficients[2,4],NA, Q10_spel,"-")  
  growth_groups[3,] <- c("Sharks and rays", nrow(shray), as.character(shraymodel$call)[1:2], coef(shraymodel), NA, summary(shraymodel)$coefficients[2,4],NA, Q10_shray,"-")  
  growth_groups[4,] <- c("Large demersal", nrow(ldem), as.character(ldemmod$call)[1:2], coef(ldemmod), NA, summary(ldemmod)$coefficients[2,4],NA, Q10_ldem,"-")  
  growth_groups[5,] <- c("Small demersal", nrow(sdem), as.character(sdemmod$call)[1:2], coef(sdemmod), NA, summary(sdemmod)$coefficients[2,4],NA, Q10_sdem,"-")  
  growth_groups[6,] <- c("Deep-living fish",nrow(deep), as.character(deepmodel$call)[1:2], coef(deepmodel), NA, summary(deepmodel)$coefficients[2,4],NA, Q10_deep,"-")  
  
  