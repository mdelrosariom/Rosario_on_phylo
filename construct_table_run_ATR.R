#For all the phylogenetic functions
library("phytools")
#open excels
library("openxlsx")
library("dplyr")
library("mvMORPH")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_2.R")
all_trees <-list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)

setwd("C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip/")

for (i in 28:length(all_trees)){
  print(i)
  tree <- read.nexus(all_trees[[1]])
  traits<-get_traits_tree(tree, all_trees[[1]])
  traits <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
  rownames(traits) <- traits$specie
  traits <- traits[, -1]
  fit1<-mvBM(tree, traits, model="BM1", method="rpf")
  

  
}
