### main analysis
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  source("Processing_fish_data.R")
  library(dplyr)
  library(lme4)
  library(nlme)
  library(lmerTest)

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

# get across species temperature (average per species)
  temp<- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Temperature"), mean, na.rm = TRUE)
  temp <- as.data.frame(temp)

  fishes <- cbind(fishes, temp[match(fishes$Name,temp$Name), c(2)])
  colnames(fishes)[25] <- "T_across"

# get within species temperature difference (obs - average per species)
  fishes$T_within <- fishes$Temperature - fishes$T_across

# M-numbers correspond to main manuscript)
####################################################  

# parameter estimation of within and across species model  
  M1    <- lmer(LArate ~ T_within + T_across + (1 | Name) + (1|uniReg) ,data=fishes, REML=T) 

# parameter estimation of difference between within and across species model  
  M1_contrast   <- lmer(LArate ~  Temperature + T_across + (1 |Name) + (1|uniReg) ,data=fishes, REML=T) 

# parameter estimation of within-between species model with random slope per species 
# convergence error (which did not happen in previous R version, used alternative optimizer)
  M2   <- lmer(LArate ~  T_within + T_across + (1 + T_within |Name) + (1|uniReg) ,data=fishes, REML=T,
             control = lmerControl(optimizer ="Nelder_Mead"))

# compare random slope/intercept with random intercept
  M1_ML <- lmer(LArate ~ T_within + T_across + (1 | Name) + (1|uniReg) ,data=fishes, REML=F)
  M2_ML <- lmer(LArate ~  T_within + T_across + (1 + T_within |Name) + (1|uniReg) ,data=fishes, REML=F)

# guild and Linf analysis of the across species effect
# get across species length (average per species)
  length <- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Linf"), mean, na.rm = TRUE)
  length <- as.data.frame(length)
  length$meanLLinf <- log10(length$Linf)
  fishes <- cbind(fishes, length[match(fishes$Name,length$Name), c(3)])
  colnames(fishes)[27] <- "across_LLinf"
  
  lmr1 <- lmer(LArate ~  T_within + T_across*grouping*across_LLinf + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  lmr2 <- lmer(LArate ~  T_within + T_across*grouping + T_across*across_LLinf  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  lmr3 <- lmer(LArate ~  T_within + T_across*grouping + grouping*across_LLinf  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  lmr4 <- lmer(LArate ~  T_within + T_across*grouping + across_LLinf  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  lmr5 <- lmer(LArate ~  T_within + T_across*across_LLinf + grouping  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  lmr6 <- lmer(LArate ~  T_within + grouping*across_LLinf + T_across  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) 
  #lmr7 <- lmer(LArate ~  T_within + grouping + across_LLinf + T_across  + (1+T_within|Name) + (1|uniReg) ,data=fishes, REML=F) # gives error, given lmr6 outcome unlikely to be best model
  AIC(lmr1,lmr2,lmr3,lmr4,lmr5,lmr6) # --> lmr2 most negative
  BIC(lmr1,lmr2,lmr3,lmr4,lmr5,lmr6)
  
  M4   <- lmer(LArate ~  T_within + T_across*grouping + T_across*across_LLinf  + (1+T_within|Name) + (1|uniReg), data=fishes, REML=T) 

  rm(list= ls()[!(ls() %in% c('M1','M1_contrast','M2','M1_ML','M2_ML','M4','fishes'))])
  
