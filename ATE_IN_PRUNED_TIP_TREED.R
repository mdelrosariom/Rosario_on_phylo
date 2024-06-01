#import libraries
library("phytools")
library("openxlsx")
library("dplyr")
library("mvMORPH")
library("tibble")#transform rownames into columns (because rownames of ATR are nodes)
#my functions
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_2.R")

#all pruned_tip
all_trees <-list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)

setwd("C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip/")

#-------------------------------------------------------------------------------

#Aphrastura_maxcred_rate_of_cytb.tre 
name_tree <- "Moho_Honeyeaters_maxcred.tre"
tree <- read.nexus(name_tree )
#function to get the traits of this species in avonet
traits<-get_traits_tree(tree, name_tree )
traits <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits) <- traits$specie
traits <- traits[, -1]
fit1<-mvBM(tree, traits, model="BM1", method="rpf")
#ATR <- estim(tree,traits, fit1, asr=TRUE)
#simulate data in NAs

complete_data <- estim(tree,traits, fit1, asr=FALSE)
#running ate
ATR<- estim(tree,complete_data$estimates, fit1, asr=TRUE)

ATE <- tibble::rownames_to_column(as.data.frame(ATR$estimates), "nodes")

variance <- tibble::rownames_to_column(as.data.frame(ATR$var), "nodes")
standard_error <- tibble::rownames_to_column(as.data.frame(ATR$var), "nodes")


file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/pruned_tip/", name_tree  , "_ATR.xlsx", sep = "")

# Export to Excel
write.xlsx(
  list(
    'ATR' =ATE,
    'variance' = variance,
    'standard error' = standard_error
    
  ),
  file = file_name
)

