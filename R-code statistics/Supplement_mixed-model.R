rm(list=ls())

### FishBase growth data statistics, supplement mixed mode analysis
###################################################################
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
  fishes$TemperatureT = 1/((8.62*10^-5)*(fishes$Temperature+273.2))

# group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf > 80] <- "LPEL"
  fishes$grouping[fishes$Func_group  == "Pelagic" & fishes$Linf <= 80] <- "SPEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic"| fishes$Func_group == "Reef"] <- "DEM"

  # get same number of observations as in main analysis
  fishes <- subset(fishes, !(is.na(fishes$Bentbio) & fishes$grouping =="DEM"))

  # finish grouping DEM
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf <= 80] <- "SDEM"
  fishes$grouping[fishes$grouping =="DEM" & fishes$Linf > 80] <- "LDEM"
  
### We analyzed the effect of temperature separately (ignoring food availability) 
  # using a mixed effect model with fish guild as random factor 
####################################################

  ### steps below:
  # 1) check growth temperature relationship using mixed model
  # 2) do the same analysis for asymptotic size
  # 3) check how influential the deep-living fish are (poor temperature range)
  # 4) summarize findings
  ####################################################
  
# 1) growth analysis
  library(nlme)

  # temperature expressed as 1/(kT) gives convergence issues in the random intercept and slope model
  # for the growth patterns, for example:
  
  wrong_lme<-lme(log(Arate) ~ TemperatureT, random = ~ 1+TemperatureT|grouping, data=fishes)  
  intervals(wrong_lme)
  
 # Error in intervals.lme(wrong_mod) : 
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
  
  good_lmer1<-lmer(log(Arate) ~ TemperatureT2 + (1 |grouping), data=fishes)  
  good_lmer2<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishes)  
  AIC(good_lmer1,good_lmer2) ## good_lmer2 best model
  
  # some diagnostics
  plot(good_lmer2)
  qqnorm(residuals(good_lmer2))
  plot(good_lmer2,factor(grouping)~resid(.))
  
  # check output and derive Q10 
  F0 <- fitted(good_lmer2, level = 0)
  I <- order(fishes$TemperatureT); Temps <- sort(fishes$TemperatureT)
  plot(Temps, F0[I])
  
  output <- coef(good_lmer2)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(Q10)
  }
    
# 2) asymptotic size analysis

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
  good_lmer1<-lmer(log(Linf) ~ TemperatureT + (1 |grouping), data=fishes)  
  good_lmer2<-lmer(log(Linf) ~ TemperatureT + (1 + TemperatureT |grouping), data=fishes)  
  AIC(good_lmer1,good_lmer2) ## approx equal models, difference is 2.36 AIC units in favour of good_lmer2 with 2 extra df
  
  # some diagnostics
  plot(good_lmer1) # gaps are probably caused since fish guilds are classified on the basis of their asymptotic size
  qqnorm(residuals(good_lmer1))
  plot(good_lmer1,factor(grouping)~resid(.))
  
  plot(good_lmer2) # gaps are probably caused since fish guilds are classified on the basis of their asymptotic size 
  qqnorm(residuals(good_lmer2))
  plot(good_lmer2,factor(grouping)~resid(.))
  
  output <- coef(good_lmer1)
  x<-c(10,20)
  x <- (1/((8.62*10^-5)*(x+273.2)))
  y<- output$grouping[1,1] + output$grouping[1,2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10<-y20/y10 
  print(Q10)
  
  output <- coef(good_lmer2)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(Q10)
  }   

  # note most effect in good_lmer2 is determined by the deep-living fish 

# 3) so check again without deep living fish
  
  fishessub<-subset(fishes,fishes$grouping != "DEEP")
  
# growth analysis
  fishessub$TemperatureT2 <- fishessub$TemperatureT/10
  
  good_lmer1<-lmer(log(Arate) ~ TemperatureT2 + (1 |grouping), data=fishessub)  
  good_lmer2<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishessub)  
  AIC(good_lmer1,good_lmer2) ## good_lmer2 best model
  
  # some diagnostics
  plot(good_lmer2)
  qqnorm(residuals(good_lmer2))
  plot(good_lmer2,factor(grouping)~resid(.))
  
  # check output and derive Q10 
  F0 <- fitted(good_lmer2, level = 0)
  I <- order(fishessub$TemperatureT2); Temps <- sort(fishessub$TemperatureT2)
  plot(Temps, F0[I])
  
  output <- coef(good_lmer2)
  for(i in 1:5){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(Q10)
  }

# asymptotic size analysis
  
  # rescaling of temperature not needed 
  good_lmer1<-lmer(log(Linf) ~ TemperatureT + (1 |grouping), data=fishessub)  
  good_lmer2<-lmer(log(Linf) ~ TemperatureT + (1 + TemperatureT |grouping), data=fishessub)  
  AIC(good_lmer1,good_lmer2) ## best model is now clearly good_lmer1
  
  # some diagnostics
  plot(good_lmer1) # gaps are probably caused since fish guilds are classified on the basis of their asymptotic size size
  qqnorm(residuals(good_lmer1))
  plot(good_lmer1,factor(grouping)~resid(.))
  
  output <- coef(good_lmer1)
  x<-c(10,20)
  x <- (1/((8.62*10^-5)*(x+273.2)))
  y<- output$grouping[1,1] + output$grouping[1,2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10<-y20/y10 
  print(Q10)
  
  # check with LME again with only random intercept
  good_lme<-lme(log(Linf) ~ TemperatureT, random = ~ 1|grouping, data=fishessub)  
  intervals(good_lme)
  
  # now coeficients are the same between LME and LMER
  coef(good_lme); coef(good_lmer1)
  
  summary(good_lme) # shows a weak effect of temperature
  
  # compare with model with only random effect, use method ML (fixed effects differ)
  good_lme1<-lme(log(Linf) ~ 1 , random = ~ 1|grouping, data=fishessub, method="ML") 
  good_lme2<-lme(log(Linf) ~ TemperatureT, random = ~ 1|grouping, data=fishessub,method="ML")  

  AIC(good_lme1,good_lme2) # model with fixed effect of temperature effect is slightly better (delta AIC = 3.1)
  
# 4) summary of the findings
  
  # growth best model with random intercept and slope 
  good_growth<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishes) 
  
  output <- coef(good_growth)
  for(i in 1:6){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(Q10)
  }   
  
  # growth without deep-living fish best model with random intercept and slope
  good_growthsub<-lmer(log(Arate) ~ TemperatureT2 + (1 + TemperatureT2 |grouping), data=fishessub) 
  
  output <- coef(good_growthsub)
  for(i in 1:5){
    x<-c(10,20)
    x <- (1/((8.62*10^-5)*(x+273.2)))/10
    y<- output$grouping[i,1] + output$grouping[i,2]*x
    y20 = exp(y[2])
    y10 = exp(y[1])
    Q10<-y20/y10 
    print(Q10)
  }   
  
  # asymptotic size best model with random intercept
  good_linf<-lmer(log(Linf) ~ TemperatureT + (1 |grouping), data=fishes)  
  
  output <- coef(good_linf)
  x<-c(10,20)
  x <- (1/((8.62*10^-5)*(x+273.2)))
  y<- output$grouping[1,1] + output$grouping[1,2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10<-y20/y10 
  print(Q10)
  
  # asymptotic size without deep-living fish best model with random intercept
  good_linfsub<-lmer(log(Linf) ~ TemperatureT + (1 |grouping), data=fishessub) 
  
  output <- coef(good_linfsub)
  x<-c(10,20)
  x <- (1/((8.62*10^-5)*(x+273.2)))
  y<- output$grouping[1,1] + output$grouping[1,2]*x
  y20 = exp(y[2])
  y10 = exp(y[1])
  Q10<-y20/y10 
  print(Q10)
  