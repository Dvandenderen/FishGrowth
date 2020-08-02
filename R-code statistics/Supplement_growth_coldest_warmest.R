rm(list=ls())

### growth data FishBase downloaded on 27 April 2018 
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/Data")
  load("Growthdata_Fishtot.Rdata")

### We classified fish following the functional group classification from the SeaAroundUs project
# When fish are not classified in the SAU project, we used the feeding type/habitat description 
# from FishBase and checked for elasmobranchs
####################################################  
  Fishclass <-read.csv("Fish_classification.csv", header=T, sep=";")
  datFish<-cbind(datFish,Fishclass[match(datFish$Name,Fishclass$Name),c(5)])
  colnames(datFish)[10] <- "Func_group"
  
  # select all growth data with tzero +- 2 (higher t0s suggest a poor fit/ a systematic error in the procedure to estimate fish age)
  # this is not the case for elasmobranchs that have a larger size at birth
  elas <- subset(datFish,datFish$Func_group =="Shark" | datFish$Func_group == "Ray")
  elas <- subset(elas,elas$Tzero <= 2 & elas$Tzero >= -5)
  
  teleost <- subset(datFish,!(datFish$Func_group =="Shark" | datFish$Func_group == "Ray"))
  teleost <- subset(teleost,teleost$Tzero <= 2 & teleost$Tzero >= -2 )
  
  datFish <- rbind(teleost,elas)  
  
  # remove species from genera Huso, Acipenser, Anguilla, Salmo and Oncorhynchus as these mainly grow in freshwater
  datFish<-subset(datFish,!(datFish$Name =="Anguilla anguilla"))
  datFish<-subset(datFish,!(datFish$Name =="Anguilla mossambica"))
  datFish<-subset(datFish,!(datFish$Name =="Huso huso"))
  datFish<-subset(datFish,!(datFish$Name =="Acipenser stellatus"))
  datFish<-subset(datFish,!(datFish$Name =="Acipenser sturio"))
  datFish<-subset(datFish,!(datFish$Name =="Salmo trutta"))
  datFish<-subset(datFish,!(datFish$Name =="Salmo salar"))
  datFish<-subset(datFish,!(datFish$Name =="Oncorhynchus clarkii"))
  datFish<-subset(datFish,!(datFish$Name =="Oncorhynchus masou masou"))
  datFish<-subset(datFish,!(datFish$Name =="Oncorhynchus mykiss"))
  
  # remove oceanic sharks following Compagno 2008 chapter 2 Pelagic Elasmobranch Diversity
  datFish <- subset(datFish,!(datFish$Name =="Rhincodon typus"))
  datFish <- subset(datFish,!(datFish$Name == "Isurus oxyrinchus"))
  datFish <- subset(datFish,!(datFish$Name == "Lamna ditropis"))
  datFish <- subset(datFish,!(datFish$Name == "Prionace glauca"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus brevipinna"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus falciformis" ))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus leucas"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus limbatus"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus longimanus"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus obscurus"))
  datFish <- subset(datFish,!(datFish$Name == "Carcharhinus plumbeus"))
  datFish <- subset(datFish,!(datFish$Name == "Galeocerdo cuvier"))
  datFish <- subset(datFish,!(datFish$Name == "Galeorhinus galeus"))
  datFish <- subset(datFish,!(datFish$Name == "Mobula japanica"))
  datFish <- subset(datFish,!(datFish$Name == "Myliobatis californica"))
  datFish <- subset(datFish,!(datFish$Name ==  "Sphyrna lewini"))
  
  ### load match between sampling locality and ecoregion (manually linked) 
  # linked to a particular marine ecoregion, or two neighboring (unknown == 0) 
  ####################################################
  Locali <-read.csv("Locations_fishBase-EcoReg.csv", header=T, sep=";")
  
  # two region names have changed while processing
  # region 1
  nb<-which (datFish$Location =="Eastern Scotian Shelf and Southern\r\nGulf of St. Lawrence, NAFO 4TVW" )
  datFish$Location[nb] <- as.character(Locali[423,1]) #"Eastern Scotian Shelf and Southern\\r\\nGulf of St. Lawrence, NAFO 4TVW "
  # region 2  
  nb<-which (datFish$Location =="off North and South Carolina (between 31°09'N and 34°44'N; \r\n 42-302 m depths)" )
  datFish$Location[nb[1]] <- as.character(Locali[1000,1])
  datFish$Location[nb[2]] <- as.character(Locali[1000,1])
  
  # link fish growth data using the location to a particular ecoregion (or two neighboring)
  datFish<-cbind(datFish,Locali[match(datFish$Location,Locali$loc),c(2:3)])
  # check which growth data did not match
  tr<- subset(datFish,is.na(datFish$Ecoregion_I)) # only data with NA Location, hence correct
  # get growth data which did match
  datFish<- subset(datFish,!(is.na(datFish$Location))) # remove all NA 
  datFish$uniReg<-paste(datFish$Ecoregion_I,datFish$Ecoregions_II,sep="_")
  datFish <-subset(datFish, datFish$uniReg !="0_0") # remove all unknown areas 
  
  # remove duplications
  datFish <-  datFish[!duplicated(datFish[,c("Name","K", "Linf","Tzero")]),]  

### This resulted in 2517 von Bertalanffy growth observations    
# from 771 different species in 165 different ecoregions.
####################################################
  obs <- nrow(datFish)
  uni_obs <- length(unique(datFish$Name))
  uni_EcReg <- length(unique(c(datFish$Ecoregion_I,datFish$Ecoregions_II))) 

### link fish growth data per ecoregion to environmental data
# most of these parameters were taken from a global earth system model (GFDL-ESM2.6) (see methods)
####################################################  
  Ecodat <-read.csv("Ecoregions_data_tempQ1_4.csv", header=T, sep=";")

  # Name_spalding = Ecoregion name following Spalding et al. 2007
  # Nb_spalding = Ecoregion number following Spalding et al. 2007
  # surftemp, each quarter = average temperature in the upper 100 meter 
  # btmtemp_shal, each quarter = the average at bottom depths < 500 meter
  # btmtemp_deep, each quarter = the average at bottom depths > 500 meter
  
  # In some ecoregions, there are no grid cells < 500 meter 
  # (the 1 degree grid is coarse for areas with a small shelf) and we predicted these values 
  
  # get relationships between Surface temp and bottom temp < 500 m
  # plot(Ecodat$surftemp~Ecodat$btmtemp_shal)
  temp <- (lm(Ecodat$btmtemp_shalQ1~Ecodat$surftempQ1))
  tempcoef <-coef(temp)
  for (i in 1:nrow(Ecodat)){
    Ecodat$btmtemp_shalQ1[i][is.na(Ecodat$btmtemp_shalQ1[i])] <- tempcoef[1]+tempcoef[2]*Ecodat$surftempQ1[i]
  }
  
  temp <- (lm(Ecodat$btmtemp_shalQ2~Ecodat$surftempQ2))
  tempcoef <-coef(temp)
  for (i in 1:nrow(Ecodat)){
    Ecodat$btmtemp_shalQ2[i][is.na(Ecodat$btmtemp_shalQ2[i])] <- tempcoef[1]+tempcoef[2]*Ecodat$surftempQ2[i]
  }
  
  temp <- (lm(Ecodat$btmtemp_shalQ3~Ecodat$surftempQ3))
  tempcoef <-coef(temp)
  for (i in 1:nrow(Ecodat)){
    Ecodat$btmtemp_shalQ3[i][is.na(Ecodat$btmtemp_shalQ3[i])] <- tempcoef[1]+tempcoef[2]*Ecodat$surftempQ3[i]
  }
  
  temp <- (lm(Ecodat$btmtemp_shalQ4~Ecodat$surftempQ4))
  tempcoef <-coef(temp)
  for (i in 1:nrow(Ecodat)){
    Ecodat$btmtemp_shalQ4[i][is.na(Ecodat$btmtemp_shalQ4[i])] <- tempcoef[1]+tempcoef[2]*Ecodat$surftempQ4[i]
  }
  
  Ecodat$min_surftemp <- apply(Ecodat[,3:6], 1, FUN=min)
  Ecodat$max_surftemp <- apply(Ecodat[,3:6], 1, FUN=max)
  Ecodat$min_btmtemp_shal <- apply(Ecodat[,7:10], 1, FUN=min)
  Ecodat$max_btmtemp_shal <- apply(Ecodat[,7:10], 1, FUN=max)
  Ecodat$min_btmtemp_deep <- apply(Ecodat[,11:14], 1, FUN=min)
  Ecodat$max_btmtemp_deep <- apply(Ecodat[,11:14], 1, FUN=max)

### now link environmental data to fish growth 
###############################################
  # first for ecoregion_I
  datFish<-cbind(datFish,Ecodat[match(datFish$Ecoregion_I,Ecodat$Nb_spalding),c(15:20)])
  colnames(datFish)[14:19]<-c("min_surftemp_I","max_surftemp_I","min_btmtemp_shal_I","max_btmtemp_shal_I","min_btmtemp_deep_I",
                              "max_btmtemp_deep_I")
  
  # now for ecoregion_II
  datFish<-cbind(datFish,Ecodat[match(datFish$Ecoregions_II,Ecodat$Nb_spalding),c(15:20)])
  
  # now get the average of the two regions
  datFish$minStemp<-rowMeans((datFish)[c("min_surftemp_I", "min_surftemp")],na.rm=T )
  datFish$maxStemp<-rowMeans((datFish)[c("max_surftemp_I", "max_surftemp")],na.rm=T )
  datFish$minBtemp<-rowMeans((datFish)[c("min_btmtemp_shal_I", "min_btmtemp_shal")],na.rm=T )
  datFish$maxBtemp<-rowMeans((datFish)[c("max_btmtemp_shal_I", "max_btmtemp_shal")],na.rm=T )
  datFish$minBtempdeep<-rowMeans((datFish)[c("min_btmtemp_deep_I", "min_btmtemp_deep")],na.rm=T )
  datFish$maxBtempdeep<-rowMeans((datFish)[c("max_btmtemp_deep_I", "max_btmtemp_deep")],na.rm=T )
  
  datFish<-datFish[,-c(14:25)]
  
  rm(list=setdiff(ls(), "datFish"))
  
  # calculate growth coefficient A = K*Linf*0.65
  datFish$Arate <- datFish$K*datFish$Linf*0.65
  
  ############ now check predictions
  library(dplyr)
  library(lme4)
  library(nlme)
  library(lmerTest)
  
  # use all observations
  fishes <- datFish
  
  # derive ambient temperature (minimum)
  fishes$Temperature <- -100
  for(i in 1:nrow(fishes)) {
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$minStemp[i] }    # temperature in first 100 meter 
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$minBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$minBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$minBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$minBtemp[i]} # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$minBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$minBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$minBtempdeep[i] } # bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$minBtempdeep[i] } # bottom temperature deeper than 500 meters
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
  
  # guild and Linf analysis of the across species effect
  # get across species length (average per species)
  length <- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Linf"), mean, na.rm = TRUE)
  length <- as.data.frame(length)
  length$meanLLinf <- log10(length$Linf)
  fishes <- cbind(fishes, length[match(fishes$Name,length$Name), c(3)])
  colnames(fishes)[27] <- "across_LLinf"
  
  M5   <- lmer(LArate ~  T_within + T_across*grouping + T_across*across_LLinf  + (1+T_within|Name) + (1|uniReg), data=fishes, REML=T) 
  
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
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[1,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[1,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # demersal
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEM","DEM")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[2,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[2,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # elasmobranchs (only 100 cm)
  grouping <- c("SHRAY","SHRAY")
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[3,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # deep
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEEP","DEEP")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[4,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data[4,1] <- 10^(yq10[2])/10^(yq10[1])

##################################################################
##### do the same for the maximum mean temperature per quarter
  fishes <- datFish
  
  # derive ambient temperature (maximum) 
  fishes$Temperature <- -100
  for(i in 1:nrow(fishes)) {
    if (fishes$Func_group[i] =="Pelagic"){fishes$Temperature[i]  <- fishes$maxStemp[i] }    # temperature in first 100 meter 
    if (fishes$Func_group[i] =="Benthopelagic"){fishes$Temperature[i]  <- fishes$maxBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Demersal"){fishes$Temperature[i]  <- fishes$maxBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Ray"){fishes$Temperature[i]  <- fishes$maxBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Flatfish"){fishes$Temperature[i]  <- fishes$maxBtemp[i]} # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Reef"){fishes$Temperature[i]  <- fishes$maxBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Shark"){fishes$Temperature[i]  <- fishes$maxBtemp[i] } # bottom temperature shallower than 500 meters
    if (fishes$Func_group[i] =="Bathypelagic"){fishes$Temperature[i]  <- fishes$maxBtempdeep[i] } # bottom temperature deeper than 500 meters
    if (fishes$Func_group[i] =="Bathydemersal"){fishes$Temperature[i]  <- fishes$maxBtempdeep[i] } # bottom temperature deeper than 500 meters
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
  
  # guild and Linf analysis of the across species effect
  # get across species length (average per species)
  length <- fishes %>% 
    group_by(Name) %>%
    summarise_at(c("Linf"), mean, na.rm = TRUE)
  length <- as.data.frame(length)
  length$meanLLinf <- log10(length$Linf)
  fishes <- cbind(fishes, length[match(fishes$Name,length$Name), c(3)])
  colnames(fishes)[27] <- "across_LLinf"
  
  M5   <- lmer(LArate ~  T_within + T_across*grouping + T_across*across_LLinf  + (1+T_within|Name) + (1|uniReg), data=fishes, REML=T) 
  
  # get Q10 output in matrix
  Q10data_max <- matrix(data = NA, ncol=2, nrow =4)
  colnames(Q10data_max) <- c("100cm","30cm")
  rownames(Q10data_max) <- c("pel","dem","shray","deep")
  
  # calculate Q10 
  Name <- rep(NA,length(2))
  uniReg <- rep(NA, length(2))
  T_within <- c(0,0)
  T_across <- c(10,20)
  
  # pelagics
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("PEL","PEL")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[1,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[1,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # demersal
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEM","DEM")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[2,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[2,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # elasmobranchs (only 100 cm)
  grouping <- c("SHRAY","SHRAY")
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[3,1] <- 10^(yq10[2])/10^(yq10[1])
  
  # deep
  across_LLinf <- c(log10(30),log10(30))
  grouping <- c("DEEP","DEEP")
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[4,2] <- 10^(yq10[2])/10^(yq10[1])
  
  across_LLinf <- c(log10(100),log10(100))
  newdat <- data.frame(T_across,Name,T_within,uniReg,grouping,across_LLinf)
  yq10 <- predict(M5, newdata = newdat, re.form = NA)
  Q10data_max[4,1] <- 10^(yq10[2])/10^(yq10[1])


