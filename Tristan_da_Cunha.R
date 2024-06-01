info_island <- island_complete_info("Tristan_da_Cunha")
list_trees_island <- info_island$syno
list_trees_island <- unique(list_trees_island[!is.na(list_trees_island)] )


for (z in 1:1){
  
  tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees/Buntings_Nesospiza_Rowettia_maxcred.tre")
  
  
  #traits<-get_traits_tree(tree,list_trees_island[[z]])
  
  traits<-  read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_Buntings_Nesospiza_Rowettia_maxcred.xlsx",1)
  
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
  
  info_tree <- subset(info_island, syno == "Buntings_Nesospiza_Rowettia_maxcred.tre")
  
  list_names <- list()
  for (i in 1:length(info_tree$colonization)){
    name <- paste("sheet_", as.character(i), sep = "") 
    list_names <- c(list_names, name)
  }
  
  for (i in 1:length(info_tree$colonization)){
    atr_specie <- traits_time_species(tree, info_tree$species[i], res, info_tree$colonization[i],list_trees_island[[z]])
    write.xlsx(atr_specie,file=paste("C:/Users/mdrmi/OneDrive/Escritorio/Tristan_da_Cunha", info_tree$species[i], "_atr_species_Fernando_Noronha.xlsx", sep =""),
               sheetName=list_names[i], append=TRUE)
  }
} 