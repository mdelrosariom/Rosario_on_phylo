library("phytools") #handling of phy objects
library("mvMORPH") #multivariate ancestral reconstruction of traits
library("phylobase") #
library("openxlsx") #open excel
library("dplyr") # dataframe handling
library("Matrix") #transform matrices to be simetrical and definite to create a usable var-cov matrix for Julien code

#functions 
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_3.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/modified_Julien_simmap_cont.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_branch_finder.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_on_time_species.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/handling_data_island.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/island_complete_info.R")
#-------------------------------------------------------------------------------

all_fixed <- list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees"
                        , pattern="\\.tre$", all.files=FALSE,
                        full.names=FALSE)

setwd("C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees")

#-------------------------------------------------------------------------------
info_island <- island_complete_info("Reunion")
    
    
list_trees_island <- info_island$syno
  
list_trees_island <- unique(list_trees_island[!is.na(list_trees_island)] )

#I still didn't figure it out how to run each tree taking into account some of them need special
#treatment because they produce an error 


#here problem because bird is not in corvides2 (the tree we selected) but in corvides 

#tree <- read.nexus(list_trees_island[[1]])

tree <- read.nexus("Terpsiphone_maxcred.tre")

#traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[1]],  ".xlsx", sep=""),1)

traits <-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", "Terpsiphone_maxcred.tre",  ".xlsx", sep=""),1)



traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]


fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(traits_atr, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[1]])

for (sp in 1:length(info_tree$species)){
  
  #atr_specie <- traits_time_species(tree, info_tree$species[[sp]], res, info_tree$colonization[[sp]],i)}
  atr_specie <- traits_time_species(tree, "Coracina_newtoni", res, 1.62,"Corvides2.tre")}


#---
tree <- read.nexus(list_trees_island[[2]])

#traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[1]],  ".xlsx", sep=""),1)

traits <-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_Fregilupus.xlsx", sep=""),1)


traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[2]])

for (sp in 1:length(info_tree$species)){
  
  #atr_specie <- traits_time_species(tree, info_tree$species[[sp]], res, info_tree$colonization[[sp]],i)}
  atr_specie <- traits_time_species(tree, "Fregilupus_varius", res, 5.76,list_trees_island[[2]])
  
  }
  
#-------------

tree <- read.nexus(list_trees_island[[3]])

traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/", list_trees_island[[3]],  ".xlsx", sep=""),1)

                          

traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[3]])

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}

for (i in 1:length(info_tree$colonization)){
  atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[3]])
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/Coracina_newtoni_tot.xlsx", info_tree$species[i], "atr_species.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)
}

#-----------------------
  
tree <- read.nexus(list_trees_island[[4]])

traits<-get_traits_tree(tree,list_trees_island[[4]])

traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/", list_trees_island[[4]],  ".xlsx", sep=""),1)



traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[3]])

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}

for (i in 1:length(info_tree$colonization)){
  atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[3]])
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)
}

#-------------------------------------------------------------------------------
tree <- read.nexus(list_trees_island[[6]])

#traits<-get_traits_tree(tree,list_trees_island[[6]])

traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[6]],  ".xlsx", sep=""),1)

traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[6]])

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}

for (i in 1:length(info_tree$colonization)){
  atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[6]])
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)
}

#-------------------------------------------------------------------------------
tree <- read.nexus(list_trees_island[[7]])

#traits<-get_traits_tree(tree,list_trees_island[[6]])

traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[7]],  ".xlsx", sep=""),1)

traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[7]])

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}

for (i in 1:length(info_tree$colonization)){
  atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[6]])
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)
}
#-------------------------------------------------------------------------------
#----------------------mew island---------------------------------------------------------

info_island <- island_complete_info("Mauritius_Island")

list_trees_island <- info_island$syno

list_trees_island <- unique(list_trees_island[!is.na(list_trees_island)] )


atr_specie <- traits_time_species(tree, "Coracina_typica", res, 1.62,"Corvides.tre")

for (z in 1:1){
  tree <- read.nexus("Corvides.tre")
  
  #traits<-get_traits_tree(tree,list_trees_island[[z]])
  
  traits<-  read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/fixed_traits/trait_Corvides.tre.xlsx")
  
  traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
  #fixes the structure of traits
  rownames(traits_atr ) <- traits_atr$specie
  traits_atr <- traits_atr[, -1]
  
  fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
  #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
  
  
  complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)
  
  ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)
  
  DaAnc <- rbind(complete_data$estimates, ATR$estimates)
  
  res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
  
  info_tree <- subset(info_island, syno ==  "Corvides2.tre")
  
  list_names <- list()
  for (i in 1:length(info_tree$colonization)){
    name <- paste("sheet_", as.character(i), sep = "") 
    list_names <- c(list_names, name)
  }
  
  for (i in 1:length(info_tree$colonization)){
    atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[z]])
    write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species.xlsx", sep =""),
               sheetName=list_names[i], append=TRUE)
  }
}





for (z in 8:8){
tree <- read.nexus(list_trees_island[[z]])

#traits<-get_traits_tree(tree,list_trees_island[[z]])

traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)

traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr ) <- traits_atr$specie
traits_atr <- traits_atr[, -1]

fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
#fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")


complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)

ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)

DaAnc <- rbind(complete_data$estimates, ATR$estimates)

res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)

info_tree <- subset(info_island, syno == list_trees_island[[z]])

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}

for (i in 1:length(info_tree$colonization)){
  atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[z]])
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)
}
}
#--------------- nueva isla rodrigues

#----------------

info_island <- island_complete_info("Rodrigues")

list_trees_island <- info_island$syno

list_trees_island <- unique(list_trees_island[!is.na(list_trees_island)] )


for (z in 1:1){
  tree <- read.nexus(list_trees_island[[z]])
  
  #traits<-get_traits_tree(tree,list_trees_island[[z]])
  
  traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
  
  traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
  #fixes the structure of traits
  rownames(traits_atr ) <- traits_atr$specie
  traits_atr <- traits_atr[, -1]
  
  fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
  #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
  
  
  complete_data <- estim(tree,traits_atr, fit1, asr=FALSE)
  
  ATR <- estim(tree, complete_data$estimates  , fit1, asr=TRUE)
  
  DaAnc <- rbind(complete_data$estimates, ATR$estimates)
  
  res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
  
  info_tree <- subset(info_island, syno == list_trees_island[[z]])
  
  list_names <- list()
  for (i in 1:length(info_tree$colonization)){
    name <- paste("sheet_", as.character(i), sep = "") 
    list_names <- c(list_names, name)
  }
  
  for (i in 1:length(info_tree$colonization)){
    atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[z]])
    write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "atr_species_RODRIGUES.xlsx", sep =""),
               sheetName=list_names[i], append=TRUE)
  }
}
