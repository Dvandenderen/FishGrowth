rm(list=ls())

### We analyzed the effect of temperature separately (ignoring food availability) 
# using a mixed effect model with fish guild or fish guild and species as random factor 
####################################################

### steps below:
# 1.1) check growth temperature relationship using mixed model with fish guild as random factor 
# 1.2) do the same analysis for asymptotic size
# 1.3) summarize findings

# 2.1) check growth temperature relationship using mixed model with fish guild and species as random factor 
# 2.2) do the same analysis for asymptotic size
# 2.3) summarize findings

####################################################

### part 1
###################################################################
  setwd(".../R-code processing")
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
  fishes$TemperatureT = 1/((8.62*10^-5)*(fishes$Temperature+273.2))

# group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf > 80] <- "LPEL"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf <= 80] <- "SPEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef"] <- "DEM"

  # get same number of observations as in main analysis
  #fishes2 <- subset(fishes, !(is.na(fishes$Bentbio) & fishes$grouping =="DEM"))

  # finish grouping DEM
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf <= 80] <- "SDEM"
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf > 80] <- "LDEM"
  
# 1.1) growth analysis
  library(nlme)

  # temperature expressed as 1/(kT) gives convergence issues
  # for the growth patterns, for example:
  
  wrong_lme<-lme(log(Arate) ~ TemperatureT, random = ~ 1+TemperatureT|grouping, data=fishes)  
  intervals(wrong_lme)
  
 # Error in intervals.lme(wrong_lme) : 
 # cannot get confidence intervals on var-cov components: Non-positive definite approximate variance-covariance
 # Consider 'which = "fixed"'
  
  # and with lmer:  
  library(lme4)
  wrong_lmer<-lmer(log(Arate) ~ TemperatureT + (1+TemperatureT |grouping), data=fishes)  
  
  # Warning message:
  # In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  # Model is nearly unidentifiable: large eigenvalue ratio
  # - Rescale variables?

# after rescaling temperature it seems OK for lmer (but not for LME,not shown)
  fishes$TemperatureT2 <- fishes$TemperatureT/10
  
  good_lmer<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishes)  

  # some diagnostics
  plot(good_lmer)
  qqnorm(residuals(good_lmer))
  plot(good_lmer,factor(grouping)~resid(.))
  
  # check output
  F0 <- fitted(good_lmer, level = 0)
  I <- order(fishes$TemperatureT); Temps <- sort(fishes$TemperatureT)
  plot(Temps, F0[I])
  
# 1.2) asymptotic size analysis

  # temperature expressed as 1/cT gives again convergence issues
  wrong_lme<-lme(log(Linf) ~ TemperatureT, random = ~ 1+TemperatureT|grouping, data=fishes)  
  intervals(wrong_lme)

  # with lmer seems to work okay:  
  good_lmer<-lmer(log(Linf) ~ TemperatureT + (1+TemperatureT |grouping), data=fishes)  
  
  # but compare with rescaling 
  fishes$TemperatureT2 <- fishes$TemperatureT/10
  good_lmer_res<-lmer(log(Linf) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishes)  
  
  # similar coeficients
  coef(good_lmer);coef(good_lmer_res) 
  
  # continue with LMER 
  good_lmer<-lmer(log(Linf) ~ TemperatureT + (1 + TemperatureT |grouping), data=fishes)  

  # some diagnostics
  plot(good_lmer) # gaps are probably caused since fish guilds are classified on the basis of their asymptotic size
  qqnorm(residuals(good_lmer))
  plot(good_lmer,factor(grouping)~resid(.))

# 1.3) summary of the findings
  
  # growth 
  good_growth<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishes) 
  
  output <- coef(good_growth)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(c(Q10, rownames(output$grouping)[i]))
  }   
  
  # asymptotic size
  good_linf<-lmer(log(Linf) ~ TemperatureT + (1 + TemperatureT |grouping), data=fishes)  
  
  output <- coef(good_linf)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(c(Q10, rownames(output$grouping)[i]))
  }   

### part 2
###################################################################
  setwd(".../R-code processing")
  source("Processing_fish_data.R")
  
  # now use all observations
  fishes <- datFish
  
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
  fishes$TemperatureT = 1/((8.62*10^-5)*(fishes$Temperature+273.2))
  
  # group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf > 80] <- "LPEL"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf <= 80] <- "SPEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef"] <- "DEM"

  # finish grouping DEM
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf <= 80] <- "SDEM"
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf > 80] <- "LDEM"

# 2.1) growth analysis
  library(nlme)
  
  # temperature expressed as 1/(kT)/10
  fishes$TemperatureT2 <- fishes$TemperatureT/10
  
  good_lmer<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping) +  (1  + TemperatureT2 |Name), data=fishes) 
  
  # some diagnostics
  plot(good_lmer)
  qqnorm(residuals(good_lmer))
  plot(good_lmer,factor(grouping)~resid(.))
  
# 2.2) asymptotic size analysis
  good_lmer<-lmer(log(Linf) ~ TemperatureT2 + (1 + TemperatureT2 |grouping) +  (1  + TemperatureT2 |Name), data=fishes) 
  
  # some diagnostics
  plot(good_lmer) # gaps are probably caused since fish guilds are classified on the basis of their asymptotic size
  qqnorm(residuals(good_lmer))
  plot(good_lmer,factor(grouping)~resid(.))
  
# 2.3) summary of the findings

  # growth 
  good_growth<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping) +  (1  + TemperatureT2 |Name), data=fishes)
  
  output <- coef(good_growth)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(c(Q10, rownames(output$grouping)[i]))
  }   
  
  # asymptotic size
  good_linf<-lmer(log(Linf) ~ TemperatureT2 + (1 + TemperatureT2 |grouping) +  (1  + TemperatureT2 |Name), data=fishes) 
  
  output <- coef(good_linf)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(c(Q10, rownames(output$grouping)[i]))
  }   
  
  # nb of observations
  table(fishes$grouping)