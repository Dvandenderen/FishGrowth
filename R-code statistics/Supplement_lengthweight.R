  rm(list=ls())

### FishBase specific length weight for each species, appendix 1 
####################################################

####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  source("Processing_fish_data.R")
  
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code processing")
  source("Resample.R")
  
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
  colnames(fishes)[21] <- "a"
  colnames(fishes)[22] <- "b"

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
 
  length  <- 5 # change between 5 and 25 cm
  fishes$newA <- fishes$a^fishes$b*(1/fishes$b)*length^(-fishes$b+1-(2/3))*fishes$K*fishes$Linf 
  
  # get log10 
  fishes$LLinf <- log10(fishes$Linf)
  fishes$LArate <- log10(fishes$newA) # now with the new coefficient A
  
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
  
  # get the prediction from M3
  meanfish  <- colMeans(M3)
  
  # get Q10 output in matrix
  Q10data <- matrix(data = NA, ncol =2, nrow =4)
  colnames(Q10data) <- c("100cm","30cm")
  rownames(Q10data) <- c("pel","deep","dem","shray")
  
  # calculate Q10 for asymptoic length 100 cm fish
  Te <- seq(0,30)
  Li <- rep(log10(100),length(Te))
  tz <- rep(0,length(Te))
  
  ypel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,1] <- (10^(ypel[31])/10^(ypel[1]))^(10/(30-0))
  
  ydeep  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,1] <- (10^(ydeep[31])/10^(ydeep[1]))^(10/(30-0))
  
  ydem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,1] <- (10^(ydem[31])/10^(ydem[1]))^(10/(30-0))
  
  yshray  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,1] <- (10^(yshray[31])/10^(yshray[1]))^(10/(30-0))
  
  # calculate Q10 for asymptoic length 30 cm fish 
  Te <- seq(0,30)
  Li <- rep(log10(30),length(Te))
  tz <- rep(0,length(Te))
  
  yspel  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingPEL"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingPEL"]*Te
  Q10data[1,2] <- (10^(yspel[31])/10^(yspel[1]))^(10/(30-0))
  
  ysdeep  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li
  Q10data[2,2] <- (10^(ysdeep[31])/10^(ysdeep[1]))^(10/(30-0))
  
  ysdem  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingDEM"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingDEM"]*Te
  Q10data[3,2] <- (10^(ysdem[31])/10^(ysdem[1]))^(10/(30-0))
  
  ysshray  <- meanfish["Intercept"]+meanfish["Temperature"]*Te+meanfish["LLinf"]*Li+
    meanfish["groupingSHRAY"]+meanfish["Tzero"]*tz + meanfish["Temperature:LLinf"]*Te*Li + 
    meanfish["Temperature:groupingSHRAY"]*Te
  Q10data[4,2] <- (10^(ysshray[31])/10^(ysshray[1]))^(10/(30-0))