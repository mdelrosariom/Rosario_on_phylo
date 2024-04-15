#For all the phylogenetic functions
library("phytools")
#open excels
library("openxlsx")
library("dplyr")
library("mvMORPH")
#GRABBING THE FUNCTION I MADE TO GER THE TRAITS OF THE SPECIES OF MY TREES FROM A LARGER 
#DATASER
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_2.R")
#ALL OF MY TREES
all_trees <-list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)

setwd("C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip/")

for (i in 1:length(all_trees)){
  
  tree <- read.nexus(all_trees[[23]])
  #MULTIPLE TRATS 
  traits<-get_traits_tree(tree, all_trees[[23]])
  #THIS CHUNK IS TO PUT SPECIES AS THE ROWNAME
  traits <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
  rownames(traits) <- traits$specie
  traits <- traits[, -1]
  #FIT THE TREES TO THE BROWNIAN MOTION MODEL, OTHERS IN MVMORPH
  fit1<-mvBM(tree, traits, model="BM1", method="rpf")
  #THE FUNCTION ESTIM MAKES TWO THINGS, IF YOU PUT asr = FALSE IT WILL 
  #SIMULATE THE NAS OF YOUR DATASET. I.E. THE MISSING MEASSUREMENTS FOR THE 
  #TRAITS OF SPECIES... (my dataset has NAs so I did this first)
  complete_data <- estim(tree,traits, fit1, asr=FALSE)
  
  #2 IF  asr= FALSE IT WILL DO THE ANCESTRAL TRAIT RECONSTRUCTION
  ATR<- estim(tree,X$estimates, fit1, asr=TRUE)
  #estimates in ATR$estimates
  
}
