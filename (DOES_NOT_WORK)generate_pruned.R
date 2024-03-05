new_pruned <- function(to_prune, name_of_tree) {
  Y_tree <- read.nexus(name_of_tree) 
  for (p in 1:length(to_prune)) {
    pr.species <- to_prune[[p]] # clades need to be a vector
    pruned_tree <- drop.tip(Y_tree, pr.species)
  }
  
  tree_name <- sub('\\.tree$', '', name_of_tree) 
  file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/pruned_trees_phase_1/", tree_name, "_pruned.tree", sep = "")
  writeNexus(pruned_tree, file = file_name)
  return(pruned_tree)
}
