
## some diagnostics on model M1 (simple) and M5 (complex)
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")

  setwd("C:/Users/pdvd/Online for git/FishGrowth/Output/Diagnostics")

# check fitted values versus residuals
  tiff(file="M1_fitted_versus_residuals.tiff", width=5, height=4.5, units="in", res=100)
  plot(M1,  resid(., type="pearson") ~fitted(.), abline =0) # == plot(M1)
  dev.off()
  
# check QQ plot and histogram (fat tails, but symmetrical)
  tiff(file="M1_QQ.tiff", width=5, height=4.5, units="in", res=100)
  qqnorm(resid(M1,type="pearson"))
  qqline(resid(M1,type="pearson"))
  dev.off()
  
  tiff(file="M1_histogram.tiff", width=5, height=4.5, units="in", res=100)
  hist(resid(M1,type="pearson"),breaks=30)
  dev.off()
  
# check pearson residuals per species
  tiff(file="M1_resid_species.tiff", width=5, height=4.5, units="in", res=100)
  plot(M1,  Name  ~ resid(., type="pearson") )
  dev.off()
      
# Check pearson resiudals per ecoregion
  tiff(file="M1_resid_regions.tiff", width=5, height=4.5, units="in", res=100)
  plot(M1,  uniReg  ~ resid(., type="pearson") )
  dev.off()
  
## M5
  
# check fitted values versus residuals
  tiff(file="M5_fitted_versus_residuals.tiff", width=5, height=4.5, units="in", res=100)
  plot(M5,  resid(., type="pearson") ~fitted(.), abline =0) # == plot(M1)
  dev.off()
  
# check QQ plot and histogram (fat tails, but symmetrical)
  tiff(file="M5_QQ.tiff", width=5, height=4.5, units="in", res=100)
  qqnorm(resid(M5,type="pearson"))
  qqline(resid(M5,type="pearson"))
  dev.off()
  
  tiff(file="M5_histogram.tiff", width=5, height=4.5, units="in", res=100)
  hist(resid(M5,type="pearson"),breaks=30)
  dev.off()
  
# check pearson residuals per species
  tiff(file="M5_resid_species.tiff", width=5, height=4.5, units="in", res=100)
  plot(M5,  Name  ~ resid(., type="pearson") )
  dev.off()
  
# Check pearson resiudals per ecoregion
  tiff(file="M5_resid_regions.tiff", width=5, height=4.5, units="in", res=100)
  plot(M5,  uniReg  ~ resid(., type="pearson") )
  dev.off()
  
# check pearson residuals versus fitted per feeding guild
  tiff(file="M5_resid_guild_versus_fitted.tiff", width=5, height=4.5, units="in", res=100)
  plot(M5,  resid(., type="pearson") ~fitted(.) | grouping, abline =0)
  dev.off()
  
# show boxplot of pearson residuals per feeding guild
  tiff(file="M5_boxplot_resid_guild.tiff", width=5, height=4.5, units="in", res=100)
  plot(M5,  grouping  ~ resid(., type="pearson"))
  dev.off()
