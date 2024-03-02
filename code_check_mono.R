'''
this will not prune. just point out the clades that can be pruned. 
'''
library("phytools") #includes ape
setwd("C:/Users/mdrmi/Downloads/Phylogenetic trees used for global analysis of island birds/Phylogenetic trees used for global analysis of island birds/Maximum clade credibility trees/")

check_monophyly <- function(tree_file_name){
  
  X_tree<-read.nexus(tree_file_name)
  
  birds <- X_tree$tip.label
  label_parts <- strsplit(birds, "_")
  common_prefix <- sapply(label_parts, function(x) paste(x[1], x[2], sep = "_"))
  common_prefix <- unique(common_prefix)
  
  all_of_sp <- list()
  
  for (j in 1:length(common_prefix)) {
    vect <- birds[grepl(common_prefix[j], birds)]
    all_of_sp <- c(all_of_sp, list(vect))
  }
  all_of_mono <- list()
  for (i in 1:length(all_of_sp)) {
    if (length(all_of_sp[[i]])>1){
      
      all_of_mono <- append(all_of_mono, list(all_of_sp[[i]]))
    }
  }
  
  
  to_prune <- c()
  to_inspect <- c()
  
  for (i in 1:length(all_of_mono)) {
    if (is.monophyletic(X_tree, all_of_mono[[i]])) {
      to_prune <- c(to_prune,list(all_of_mono[[i]]) )
    } else {
      to_inspect <- c(to_inspect, list(all_of_mono[[i]]))
    }
  }
  #to inspect 11, to prune 30, to now good. 
  lost_case_ask_ben <- list()
  
  still_hope <- list()
  
  
  
  
  lost_case_ask_ben <- list()
  still_hope <- list()
  
  for (z in 1:length(to_inspect)) {
    label_parts <- strsplit(as.character(to_inspect[[z]]), "_")
    common_prefix <- sapply(label_parts, function(x) paste(x[3], sep = "_"))
    
    if (length(common_prefix) == length(unique(common_prefix))) {
      lost_case_ask_ben <- c(lost_case_ask_ben, list(to_inspect[[z]]))
    } else {
      still_hope <- c(still_hope, list(to_inspect[[z]]))
    }
  }
  return(list(monophyletic_clades = all_of_mono, prunable_clades = to_prune, 
              possibly_monophyletic = still_hope, ask_ben = lost_case_ask_ben))
}


    
    
    
