source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/handling_data_island.R")

island_complete_info <- function(name_of_island){
  all_fixed <- list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees"
                          , pattern="\\.tre$", all.files=FALSE,
                          full.names=FALSE)
  info_island <- preparing_data(name_of_island) 
  trees_island <- unique(info_island$tree)
  for (o in 1:length(trees_island)) {
    if (grepl("_", trees_island[o])) { 
      separate <- strsplit(trees_island[o], "_")
      trees_island[o] <- separate[[1]][1]
    }
  }
  
  available_trees <- list()
  
  for (i in trees_island) {
    for (j in all_fixed) {
      if (grepl(i, j)) {
        
        available_trees <- c(available_trees, j)}}}
  
  
  available_trees <- unlist(available_trees)
  
  info_island$syno <- NA
  
  
  
  for (i in 1:length(info_island$tree)) {
    for (j in available_trees) {
      if (grepl("_", info_island$tree[[i]])) { 
        separate <- strsplit(info_island$tree[[i]], "_")
        compare <- separate[[1]][1]
        if(grepl( compare, j)){
          info_island$syno[[i]] <-j 
          
        }}else{if(grepl( info_island$tree[[i]], j)){
          info_island$syno[[i]] <-j 
          
          
        }}}}
  return(info_island)
  
}