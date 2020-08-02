### main analysis - phylogenetic model
####################################################
  setwd("C:/Users/pdvd/Online for git/FishGrowth/R-code statistics")  
  source("Main_analyses.R")
  library(rotl)
  library(ape)
  library(fishtree)
  library(phangorn)
  library(MCMCglmm)

# rename some fish to match with fish tree of life
  fishes$Name[fishes$Name == "Clupea pallasii suworowi"] <- "Clupea pallasii"
  fishes$Name[fishes$Name == "Belone euxini"] <- "Belone belone"
  fishes$Name[fishes$Name == "Osmerus dentex"] <- "Osmerus mordax"
  fishes$Name[fishes$Name == "Gadiculus thori"] <- "Gadiculus argenteus"
  fishes$Name[fishes$Name == "Etrumeus golanii"] <- "Etrumeus golani"
  fishes$Name[fishes$Name == "Chelidonichthys lucerna"] <- "Chelidonichthys lucernus"

# get unique fish    
  fishN <- unique(fishes$Name)
  
  ftree <- fishtree_phylogeny(species = fishN)
  is.ultrametric(ftree)
  ftree <- nnls.tree(cophenetic(ftree),ftree, rooted=TRUE,trace=0)
  is.ultrametric(ftree)
  
# clean the names
  ftree$tip.label <- strip_ott_ids(ftree$tip.label, remove_underscores = TRUE)
  
# prepare the tree for the mixed model
  Ainv <-inverseA(ftree)$Ainv

# now get the dataframe all species with phylogenetic information 
  fish_phylo <- subset(fishes,fishes$Name%in% c(ftree$tip.label))
  
# set priors
  p.var<-var(fish_phylo$LArate)
  prior_Ps <-list(R =list(V = p.var/2, nu = 0.002),
                  G =list(G1 =list(V =diag(2), nu = 2,alpha.mu =rep(0, 2),alpha.V=diag(25^2, 2, 2)),
                          G2 = list(V = 1, nu = 0.002)))
  
# run the phylogenetic model (M4) for data subst (fish_phylo)
  nitt <- 5e4
  burnin <- 1000
  thin <- nitt/1000
  M3_phylo <- MCMCglmm(LArate ~ T_within + T_across,random=~us(1+T_within):Name + uniReg,
                        ginverse=list(Name=Ainv),
                        data=fish_phylo,prior = prior_Ps,
                        nitt=nitt,burnin=burnin,
                        thin=thin,verbose=FALSE)
  
  summary(M3_phylo) 
  T_across <- c(10,20)
  yq10 <- 0.008181*T_across
  10^(yq10[2])/10^(yq10[1])
  yq10_low <- 0.004456*T_across
  10^(yq10_low[2])/10^(yq10_low[1])
  yq10_high <- 0.012273*T_across
  10^(yq10_high[2])/10^(yq10_high[1])
  
# compare with model without phylogeny based on data subset (fish_phylo)
  nitt <- 5e4
  burnin <- 1000
  thin <- nitt/1000
  M3_nophylo <- MCMCglmm(LArate ~ T_within + T_across, random=~us(1 + T_within):Name + uniReg,
                        data=fish_phylo, prior = prior_Ps,
                        nitt=nitt, burnin=burnin,
                        thin=thin, verbose=FALSE)
  summary(M3_nophylo) 
  
  T_across <- c(10,20)
  yq10 <- 0.013395*T_across
  10^(yq10[2])/10^(yq10[1])
  yq10_low <- 0.010258*T_across
  10^(yq10_low[2])/10^(yq10_low[1])
  yq10_high <- 0.016574*T_across
  10^(yq10_high[2])/10^(yq10_high[1])
  
# check if fixed effects are the same with lmer
  lmr_nophylo <- lmer(LArate ~  T_within + T_across + (1 + T_within | Name) +  (1|uniReg) ,data=fish_phylo, REML=T) 
  fixef(lmr_nophylo)

# run M4 with phylogenetic distance matrix resulting in appendix 1, table S1.5 fourth column
  nitt <- 5e4
  burnin <- 1000
  thin <- nitt/1000
  M5_phylo <- MCMCglmm(LArate ~ T_within + T_across*grouping + T_across*across_LLinf, random=~us(1+T_within):Name + uniReg,
                       ginverse=list(Name=Ainv),
                       data=fish_phylo,prior = prior_Ps,
                       nitt=nitt,burnin=burnin,
                       thin=thin,verbose=FALSE)

  temp <- c(10,20)
  slope_temp <- 0.026290
  pel_temp <- 0.005939
  dem_temp <- 0.002048
  linf_temp <- -0.011605
  
  # Pel 30 cm
  yq10 <- slope_temp*temp + pel_temp*temp + linf_temp*log10(30)*temp
  10^(yq10[2])/10^(yq10[1])
  
  # Pel 100 cm
  yq10 <- slope_temp*temp + pel_temp*temp + linf_temp*log10(100)*temp
  10^(yq10[2])/10^(yq10[1])

  
  # Dem 30 cm
  yq10 <- slope_temp*temp + dem_temp*temp + linf_temp*log10(30)*temp
  10^(yq10[2])/10^(yq10[1])
  
  # Dem 100 cm
  yq10 <- slope_temp*temp + dem_temp*temp + linf_temp*log10(100)*temp
  10^(yq10[2])/10^(yq10[1])
  
  # elasmo 30 - 100 cm
  # not available

  # Deep 30 cm
  yq10 <- slope_temp*temp + linf_temp*log10(30)*temp
  10^(yq10[2])/10^(yq10[1])
  
  # Deep 100 cm
  yq10 <- slope_temp*temp + linf_temp*log10(100)*temp
  10^(yq10[2])/10^(yq10[1])
  