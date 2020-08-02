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
Ecodat <-read.csv("Ecoregions_data.csv", header=T, sep=";")
 
  # Name_spalding = Ecoregion name following Spalding et al. 2007
  # Nb_spalding = Ecoregion number following Spalding et al. 2007
  # zoo_bio = Zooplankton prey biomass COBALT model mg C/m2
  # zoo_prod = Zooplankton prey production COBALT model mg C/m2/day
  # surftemp = average temperature in the upper 100 meter 
  # btmtemp_shal = the average at bottom depths < 500 meter
  # btmtemp_deep = the average at bottom depths > 500 meter
  # NPP = average daily net primary production (mg C/m2/day) 

  Ecodat$zoo_bio <- Ecodat$zoo_bio/1000 # gr C/m2
  Ecodat$zoo_prod <- Ecodat$zoo_prod/1000 # gr C/m2/day
  Ecodat$NPP <- Ecodat$NPP/1000 # gr C/m2/day

# In some ecoregions, there are no grid cells < 500 meter 
# (the 1 degree grid is coarse for areas with a small shelf) and we predicted these values 
  
  # get relationships between Surface temp and bottom temp < 500 m
  # plot(Ecodat$surftemp~Ecodat$btmtemp_shal)
  temp <- (lm(Ecodat$btmtemp_shal~Ecodat$surftemp))
  tempcoef <-coef(temp)
  for (i in 1:nrow(Ecodat)){
    Ecodat$btmtemp_shal[i][is.na(Ecodat$btmtemp_shal[i])] <- tempcoef[1]+tempcoef[2]*Ecodat$surftemp[i]
  }
  
### now link environmental data to fish growth 
###############################################
  # first for ecoregion_I
  datFish<-cbind(datFish,Ecodat[match(datFish$Ecoregion_I,Ecodat$Nb_spalding),c(3:8)])
  colnames(datFish)[14:19]<-c("zoo_bio_I","zoo_prod_I","surftemp_I","btmtemp_shal_I","btmtemp_deep_I",
                              "NPP_I")
  
  # now for ecoregion_II
  datFish<-cbind(datFish,Ecodat[match(datFish$Ecoregions_II,Ecodat$Nb_spalding),c(3:8)])
  
  # now get the average of the two regions
  datFish$Zoobio<-rowMeans((datFish)[c("zoo_bio_I", "zoo_bio")],na.rm=T )
  datFish$Zooprod<-rowMeans((datFish)[c("zoo_prod_I", "zoo_prod")],na.rm=T )
  datFish$Stemp<-rowMeans((datFish)[c("surftemp_I", "surftemp")],na.rm=T )
  datFish$Btemp<-rowMeans((datFish)[c("btmtemp_shal_I", "btmtemp_shal")],na.rm=T )
  datFish$Btempdeep<-rowMeans((datFish)[c("btmtemp_deep_I", "btmtemp_deep")],na.rm=T )
  datFish$NPProd<-rowMeans((datFish)[c("NPP_I", "NPP")],na.rm=T )

  datFish<-datFish[,-c(14:25)]
  
  rm(list=setdiff(ls(), "datFish"))
        
# calculate growth coefficient A = K*Linf*0.65
  datFish$Arate <- datFish$K*datFish$Linf*0.65

