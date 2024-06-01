#Aldabra_Group

info_island <- island_complete_info("Aldabra_Group")
list_trees_island <- info_island$syno
list_trees_island <- unique(list_trees_island[!is.na(list_trees_island)] )
#----- Columbiformes 

tree <- read.nexus("Columbiformes_maxcred.tre")
traits <-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", "Columbiformes_maxcred.tre",  ".xlsx", sep=""),1)

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

info_tree <- subset(info_island, syno == "Columbiformes_maxcred.tre")

list_names <- list()
for (i in 1:length(info_tree$colonization)){
  name <- paste("sheet_", as.character(i), sep = "") 
  list_names <- c(list_names, name)
}


  atr_specie <- traits_time_species(tree, "Nesoenas_picturata", res, 0.29,"Columbiformes_maxcred.tre")
  write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", "Nesoenas_picturata", "_atr_Aldabra_Group.xlsx", sep =""),
             sheetName=list_names[i], append=TRUE)


#--------------------------------------------------SUNBIRDS----------------
  
  
  for (z in 2:2){
    
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
      atr_specie <- traits_time_species(tree, "Cinnyris.souimanga.aldabrensis.ALDABRA.AY235547" , res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
  
#-------------------------------------------------Cisticola_maxcred.tre
  
  for (z in 3:3){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <- traits_atr #not missing data 
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, info_tree$species[i] , res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
  
#---------------------------------------------------------------CURVUS
  
  for (z in 4:4){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <- traits_atr #not missing data 
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, info_tree$species[i] , res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
 
#---------------Dicrurus 
  for (z in 5:5){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <- traits_atr #not missing data 
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, info_tree$species[i] , res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
  
#---------------------------------------------Foudia
  
  for (z in 6:6){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <- traits_atr #not missing data 
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, "FoudiaeminentissimaaldabranaALDABRA341a" , res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
  
  
#------------------------------------------------------Hypsipetes_maxcred_rate_of_cytb.tre
  
  
  for (z in 7:7){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <- traits_atr #not missing data 
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, "Hypsipetes.madagascariensis.rostratus.ALDABRA.AY590750", res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
#------------------------------------------------------Nesillas.tre
  
  
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
    
    
    complete_data <- estim(tree, complete_data  , fit1, asr=FALSE)
    
    ATR <- estim(tree, complete_data  , fit1, asr=TRUE)
    
    DaAnc <- rbind(complete_data$estimates, ATR$estimates)
    
    res<- mvbrownian_path(as.matrix(DaAnc), tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)
    
    info_tree <- subset(info_island, syno == list_trees_island[[z]])
    
    list_names <- list()
    for (i in 1:length(info_tree$colonization)){
      name <- paste("sheet_", as.character(i), sep = "") 
      list_names <- c(list_names, name)
    }
    
    for (i in 1:length(info_tree$colonization)){
      atr_specie <- traits_time_species(tree, "Nesillas_aldabranus", res, info_tree$colonization[i],list_trees_island[[z]])
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "WIERD_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  }
  
#---------------------------------------ZORO
 
  for (z in 9:9){
    
    tree <- read.nexus(list_trees_island[[z]])
    
    z<-9
    #traits<-get_traits_tree(tree,list_trees_island[[z]])
    
    traits<-  read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", list_trees_island[[z]],  ".xlsx", sep=""),1)
    
    traits_atr <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
    #fixes the structure of traits
    rownames(traits_atr ) <- traits_atr$specie
    traits_atr <- traits_atr[, -1]
    
    fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
    #fit1<-mvgls(as.matrix(traits_atr)~1, data=as.data.frame(traits_atr), tree, model="BM", penalty="RidgeArch")
    
    
    complete_data <-estim(tree, traits_atr , fit1, asr=FALSE)
    
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
      write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/", info_tree$species[i], "_atr_species_Aldabra_Group.xlsx", sep =""),
                 sheetName=list_names[i], append=TRUE)
    }
  } 
  
  
#--------------------------------------------------
  