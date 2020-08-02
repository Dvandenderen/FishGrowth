  rm(list=ls())

### FishBase specific length weight for each species, appendix 1 
####################################################

####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  source("Processing_fish_data.R")

  fishes <- datFish

### get unique species and use FishBase for length weight
####################################################
  library(rfishbase)
  
  uni_spec <- unique(datFish$Name)
  uniFish <- length_weight(paste(uni_spec)) 
  uniFish <- as.data.frame(uniFish)

  uniFish$a <- as.numeric(uniFish$a)  
  uniFish$b <- as.numeric(uniFish$b)
  
  # aggregate by mean of the length weight per species
  library(dplyr)
  fishLW<-uniFish %>% 
    group_by(Species) %>%
    summarise_at (c("a","b"), mean, na.rm = TRUE)
  fishLW <- as.data.frame(fishLW)

  fishes <- cbind(fishes,fishLW[match(datFish$Name,fishLW$Species),c(2:3)])

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
  
  # group fish guilds
  fishes$grouping <- "NA"
  fishes$grouping[fishes$Func_group  == "Pelagic" ] <- "PEL"
  fishes$grouping[fishes$Func_group == "Shark" | fishes$Func_group == "Ray"] <- "SHRAY"
  fishes$grouping[fishes$Func_group == "Bathypelagic" | fishes$Func_group == "Bathydemersal"] <- "DEEP"
  fishes$grouping[fishes$Func_group == "Demersal" | fishes$Func_group == "Flatfish" | fishes$Func_group == "Benthopelagic" | fishes$Func_group == "Reef"] <- "DEM"
  
  # calculate A for all species where there is data
  fishes <- subset(fishes,!(is.na(fishes$a)))
  fishes$b <- 1/fishes$b
 
  # now do it for 2 lengths, 5 and 25
  length  <- 25
  fishes$newA <- fishes$a^fishes$b*(1/fishes$b)*length^(-fishes$b+1-(2/3))*fishes$K*fishes$Linf 
  
  # get log10 
  fishes$LLinf  <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$newA)
  
  # get across species temperature (average per species)
  temp<- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Temperature"), mean, na.rm = TRUE)
  temp <- as.data.frame(temp)
  
  fishes <- cbind(fishes, temp[match(fishes$Name,temp$Name), c(2)])
  colnames(fishes)[28] <- "T_across"
  
  # get within species temperature difference (obs - average per species)
  fishes$T_within <- fishes$Temperature - fishes$T_across
  
  # get across species length (average per species)
  length <- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Linf"), mean, na.rm = TRUE)
  length <- as.data.frame(length)
  length$meanLLinf <- log10(length$Linf)
  fishes <- cbind(fishes, length[match(fishes$Name,length$Name), c(3)])
  colnames(fishes)[30] <- "across_LLinf"
  
  library(lme4)
  # fit model
  M4_LW   <- lmer(LArate ~  T_within + T_across*grouping + T_across*across_LLinf  + (1+T_within|Name) 
                  + (1|uniReg), data=fishes, REML=T, control = lmerControl(optimizer ="Nelder_Mead")) 
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol=2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","dem","shray","deep")
  
  # calculate Q10 
  Name <- rep(NA,length(2))
  uniReg <- rep(NA, length(2))
  T_within <- c(0,0)
  T_across <- c(10,20)
  
  # pelagics
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("PEL","PEL")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[1,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[1,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # demersal
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEM","DEM")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[2,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[2,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # elasmobranchs (only 100 cm)
  grouping <- c("SHRAY","SHRAY")
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[3,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # deep
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEEP","DEEP")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[4,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M4_LW, newdata = newdat, re.form = NA)
  Q10data[4,1] <- 10^(yq10[2])/10^(yq10[1])
  