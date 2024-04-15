#For all the phylogenetic functions
library("phytools")
#ID remember why this is in here :) 
library("SwimmeR")
#open excels
library("openxlsx")
#manipulate strings
library("stringr")

#my functions
#to categorize the tree with respect of problems of monophyly, etc
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/check_monophyly.R")
#export the data produced by check_mono. be careful because function called just export_mono
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/export_mono_2.R")
#tp prune the prunabble (monophyletic) branches
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/generate_pruned.R")
#to change names of the tree
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/change_tips.R")

#we will need to change this because we need to download each tree by hand :))))
setwd("C:/Users/mdrmi/Downloads/Phylogenetic trees used for global analysis of island birds/Phylogenetic trees used for global analysis of island birds/Maximum clade credibility trees/")
#same, but for now "all trees" contain, well... all trees
all_trees <-list.files(path="C:/Users/mdrmi/Downloads/Phylogenetic trees used for global analysis of island birds/Phylogenetic trees used for global analysis of island birds/Maximum clade credibility trees/"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)

#to check names and do ATR
direct_ATR <- list()
#For changing names, prune and do ATR
prune_and_ATR <- list()
#check mono manually
check_mono_manually <- list()
#neither 
others <- list()


for (i in 1:20)
  {
  X_tree <- read.nexus(all_trees[[i]])
  #check_monophyly and export 
  tree_analyzed <- check_monophyly(all_trees[[i]])
  export_mono(tree_analyzed, all_trees[[i]])
  
  # classification in the lists we mentioned before 
  if (length(tree_analyzed$all_of_mono) == 0 | is.null(as.vector(unlist(tree_analyzed$all_of_mono)))) { 
    direct_ATR <- c(direct_ATR, list(all_trees[[i]]))
  } else if ((length(tree_analyzed$lost_case_ask_ben) == 0 & length(tree_analyzed$still_hope) == 0) |
             (is.null(as.vector(unlist(tree_analyzed$lost_case_ask_ben))) & is.null(as.vector(unlist(tree_analyzed$still_hope))))) {
    prune_and_ATR <- c(prune_and_ATR, list(all_trees[[i]]))
  } else if (length(tree_analyzed$lost_case_ask_ben) == 0 | is.null(as.vector(unlist(tree_analyzed$lost_case_ask_ben)))) { 
    check_mono_manually <- c(check_mono_manually, list(all_trees[[i]]))
  } else {
    others <- c(others, list(all_trees[[i]]))
  }
  
  direct_ATR <-(list(unlist(direct_ATR)))
  prune_and_ATR <- (list(unlist(prune_and_ATR)))
  check_mono_manually <- list(unlist(check_mono_manually))
  others <- list(unlist(others))
  #exported just to have the info 
  write.xlsx(
    list(
      'direct_ATR' = direct_ATR,
      'prune_and_ATR' = prune_and_ATR,
      'check_mono_manuelly' = check_mono_manually,
      'others' = others
    ),
    file = "C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/classification_trees.xlsx"
  )
  
 if (all_trees[[i]] %in% direct_ATR) { 
   X_tree <- change_tips(X_tree)
 } 
  
  if  (all_trees[[i]] %in%  prune_and_ATR) { 
    #prune the tree
    new_pruned(tree_analyzed, all_trees[[i]])
    #change name of the trees 
    X_tree <- change_tips(X_tree)
    
  } 
  
  
  
  
  
  
}
#First case (idt that we have it, but just in case) change name and run ATR 





