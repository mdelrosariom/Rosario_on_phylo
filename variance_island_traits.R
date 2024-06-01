variance_island <- function(island_name, all_fixed) {
  info_island <- preparing_data(island_name) 
  trees_island <- unique(info_island$tree)
  
  # Clean tree names if needed
  for (o in 1:length(trees_island)) {
    if (grepl("_", trees_island[o])) { 
      separate <- strsplit(trees_island[o], "_")
      trees_island[o] <- separate[[1]][1]
    }
  }
  
  
  for (i in trees_island) {
    for (j in all_fixed) {
      if (grepl(i, j)) {
        tree <- read.nexus(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_fixedtrees/", j, sep= ""))
        
        traits <- read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/",j, ".xlsx", sep =""),1)
        traits<- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
        #fixes the structure of traits
        rownames(traits_atr ) <- traits_atr$specie
        traits_atr <- traits_atr[, -1]
        
        
        fit1<-mvBM(tree, traits_atr,  model="BM1", method="rpf")
        complete_data <- estim(tree, traits_atr, fit1, asr = FALSE) }}}
       