library("phytools")
library("SwimmeR")
setwd("C:/Users/mdrmi/Downloads/Phylogenetic trees used for global analysis of island birds/Phylogenetic trees used for global analysis of island birds/Maximum clade credibility trees/")

check_monophyly <- function(tree_file_name) {
  X_tree <- read.nexus(tree_file_name)
  birds <- X_tree$tip.label #we work with the names of the tips
  
  label_parts <- strsplit(birds, "_") # separate the names by the subscore
  common_prefix <- sapply(label_parts, function(x) paste(x[1], x[2], sep = "_")) #check the name of the sps. 
  common_prefix <- unique(common_prefix) #as sps. are repeated.
  
  all_of_sp <- lapply(common_prefix, function(prefix) birds[grepl(prefix, birds)]) #combine in a list the same species
  
  all_of_mono <- all_of_sp[sapply(all_of_sp, function(x) length(x) > 1)] #if there are repeated sps.
  
  to_prune <- lapply(all_of_mono, function(x) if (is.monophyletic(X_tree, x)) x else NA) #monophyletic species can be pruned
  still_hope <- lapply(all_of_mono, function(x) if (!is.monophyletic(X_tree, x)) x else NA) #this sps are divided into subspecies or by geographical region
  #but their separation is too complicated and not wort it, so we need to check for monophyly by hand
  lost_case_ask_ben <- lapply(still_hope, function(x) { #sps can have a taxonomic problem, so we need to ask ben or papers
    if (length(unique(unlist(strsplit(as.character(x), "_")))) == length(unlist(strsplit(as.character(x), "_")))) x else NA
  })
  
  return(data.frame(
    monophyletic_clades = I(all_of_mono), #I() because if not R will try to modify the lists, this is used when lists have different size. 
    prunable_clades = I(to_prune),
    possibly_monophyletic = I(still_hope),
    ask_ben = I(lost_case_ask_ben)
  ))
}
