#************************************************************************
# R script for analysing lake data using joint species distribution models 
#************************************************************************

#************************************************************************
# Packages####
pacman::p_load(Hmsc,MASS,tidyverse, dplyr, fields, ape, knitr)
#************************************************************************


#************************************************************************
# load data ####
load("data/output/mother_lake.RData") # data called "chloro_chem_mnth_pheno"
#************************************************************************

master_lake = chloro_chem_mnth_pheno

nChains = 2 # two chains
thin = 5 # record every 5th step of the iteration
samples = 100 # how many samples to obtain per chain
transient = 500*thin # how long transient (burn-in) to include
verbose = 500*thin # how frequently to see progress of MCMC sampling


# test model ####
#fixed effects: the NO2NO3 and water temp (continuous). 
#To allow for the possibility of an intermediate thermal optimum, we also include the squared effect of temp 
# Species traits: multicellularity
# not including phylogeny for now
XData = data.frame(no2no3 = master_lake$NO2NO3_ug.l, temp = master_lake$water_temp)
XFormula = ~no2no3 + poly(temp,degree = 2,raw = TRUE)
TrFormula = ~master_lake$multi
studyDesign = data.frame(sample = sprintf('sample_%.3d',1:n), stringsAsFactors=TRUE) 
rL = HmscRandomLevel(units = studyDesign$sample)
rL$nfMax = 15
m = Hmsc(Y = Y, XData = XData, XFormula = XFormula,
         TrData = traits, TrFormula = TrFormula,
         phyloTree = phy,
         studyDesign = studyDesign, ranLevels = list(sample = rL))