#For all the phylogenetic functions
library("phytools")
#ID remember why this is in here :) 
library("SwimmeR")
#open excels
library("openxlsx")
#manipulate strings
library("stringr")
############################ Acrocephalus_maxcred.tre
#to check mono
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/UPDATED_check_monophyly.R")
#tp prune the prunabble (monophyletic) branches
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/generate_pruned.R")
#to delete still hope 
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/drop_still_hope.R")
#to change tips
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/change_tips.R")
setwd("C:/Users/mdrmi/OneDrive/Escritorio/trees")
#same, but for now "all trees" contain, well... all trees (not as before that ommited other)
all_trees <-list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/trees"
                       , pattern="\\.tre$", all.files=FALSE,
                       full.names=FALSE)

#check mono
tree_analyzed <- check_monophyly(all_trees[[1]])
pruned_of_mono_groups <- new_pruned(tree_analyzed, all_trees[[1]])


tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/trees_reunion_703/Acrocephalus_maxcred.tre_pruned.tree")
#for the Acrocephalus_aedon_AF094623
tree<-drop.tip(tree, tree_analyzed$lost_case_ask_ben[[1]])
#aviceniae
to_prune <- tree_analyzed$lost_case_ask_ben[[2]]
to_prune <- to_prune[to_prune != to_prune[[1]]]
tree <- drop.tip(tree, to_prune)
# Acrocephalus_vaughani

to_prune <-  tree_analyzed$lost_case_ask_ben[[4]]
to_prune <- to_prune[to_prune != to_prune[[1]]]
tree <- drop.tip(tree, to_prune)

#"Alaudidae_maxcred.tre"

tree_analyzed <- check_monophyly(all_trees[[2]])
pruned_of_mono_groups <- new_pruned(tree_analyzed, all_trees[[2]])

tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/trees_reunion_703/Alaudidae_maxcred.tre_pruned.tree")

tree$tip.label <- new_labels

#Anairetes_maxcred.tre
tree<- read.nexus("Anairetes_maxcred.tre")
tree_analyzed <- check_monophyly("Anairetes_maxcred.tre")
pruned_of_mono_groups <- new_pruned(tree_analyzed, "Anairetes_maxcred.tre")
tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/trees_reunion_703/Anairetes_maxcred.tre_pruned.tree")
#Anthus_maxcred.tre
tree<- read.nexus("Anthus_maxcred.tre")
tree_analyzed <- check_monophyly("Anthus_maxcred.tre")
pruned_of_mono_groups <- new_pruned(tree_analyzed, "Anthus_maxcred.tre")

tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/trees_reunion_703/Anthus_maxcred.tre_pruned.tree")
#still hope 
is.monophyletic(tree,"Anthus_rufulus_India_KJ702923", "Anthus_rufulus_India_KJ702925"   )
#T

#Buntings_Nesospiza_Rowettia_maxcred.tre
tree<- read.nexus("Buntings_Nesospiza_Rowettia_maxcred.tre")
tree_analyzed <- check_monophyly("Buntings_Nesospiza_Rowettia_maxcred.tre")
pruned_of_mono_groups <- new_pruned(tree_analyzed, "Anthus_maxcred.tre")

#Carduelis_maxcred.tre
tree<- read.nexus("Buntings_Nesospiza_Rowettia_maxcred.tre")
tree_analyzed <- check_monophyly("Buntings_Nesospiza_Rowettia_maxcred.tre")

#################################################################################
#Cisticola_maxcred.tre
tree<- read.nexus("Cisticola_maxcred.tre")
tree_analyzed <- check_monophyly("Cisticola_maxcred.tre")

is.monophyletic(tree, "Cisticola_juncidis_AJ004314", "Cisticola_juncidis_HQ608851",
                +                 "Cisticola_juncidis_Z73474", "Cisticola_juncidis_juncidis_DQ008493" )
#[1] TRUEx
is.monophyletic( tree, "Cisticola_juncidis_brunniceps_AB239499", 
                 +                  "Cisticola_juncidis_brunniceps_AB239500",  "Cisticola_juncidis_brunniceps_AB239501" )
#[1] TRUEx
is.monophyletic(tree,"Cisticola_juncidis_tinnabulans_AB239502",
                + "Cisticola_juncidis_tinnabulans_AB239503", "Cisticola_juncidis_tinnabulans_AB239504")
#[1] TRUE

#first prune monophyletic

pruned_of_mono_groups <- new_pruned(tree_analyzed, "Cisticola_maxcred.tre")
tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/trees_reunion_703/Cisticola_maxcred.tre_pruned.tree")

##prune the still hope 
vector<- c("Cisticola_juncidis_AJ004314", "Cisticola_juncidis_HQ608851",
                           "Cisticola_juncidis_Z73474", "Cisticola_juncidis_juncidis_DQ008493" )
tree <-drop_still_hope(vector,tree)

vector2 <- c("Cisticola_juncidis_brunniceps_AB239499", 
                              "Cisticola_juncidis_brunniceps_AB239500",  "Cisticola_juncidis_brunniceps_AB239501" )
tree<-drop_still_hope(vector2,tree)

vector3 <- c("Cisticola_juncidis_tinnabulans_AB239502",
"Cisticola_juncidis_tinnabulans_AB239503", "Cisticola_juncidis_tinnabulans_AB239504")
tree<- drop_still_hope(vector3, tree)

tree <-change_tips(tree)

writeNexus(tree, file="C:/Users/mdrmi/OneDrive/Escritorio/trees_fixed/Cisticola_maxcred.tre")

#######################################################################################
#Clytorhynchus_maxcred.tre
tree <- read.nexus("Clytorhynchus_maxcred.tre")
tree_analyzed <- 