

#my functions
#to categorize the tree with respect of problems of monophyly, etc
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/UPDATED_check_monophyly.R")
#export the data produced by check_mono. be careful because function called just export_mono
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/export_mono_finalish_version.R")
#tp prune the prunabble (monophyletic) branches
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/generate_pruned.R")
#to change names of the tree
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/change_tips.R")
#to get traits per tree
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits.R")
#classification of trees 
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/classifier.R")

#we will need to change this because we need to download each tree by hand :))))
setwd("C:/Users/mdrmi/OneDrive/Escritorio/trees")
#same, but for now "all trees" contain, well... all trees (not as before that ommited other)
all_trees <-list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/trees"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)





  










