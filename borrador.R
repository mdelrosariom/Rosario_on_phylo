classify_tree <- function(tree){

if (length(tree_analyzed$all_of_mono) == 0 | is.null(as.vector(unlist(tree_analyzed$all_of_mono)))) { 
  direct_ATR <- c(direct_ATR, list(all_trees[[i]]))
  
} else if ((length(tree_analyzed$lost_case_ask_ben) == 0 & length(tree_analyzed$still_hope) == 0) |
           (is.null(as.vector(unlist(tree_analyzed$lost_case_ask_ben))) & is.null(as.vector(unlist(tree_analyzed$still_hope))))) {
  prune_and_ATR <- c(prune_and_ATR, list(all_trees[[i]]))
} else if (length(tree_analyzed$lost_case_ask_ben) == 0 | is.null(as.vector(unlist(tree_analyzed$lost_case_ask_ben)))) { 
  check_mono_manually <- c(check_mono_manually, list(all_trees[[i]]))
} else {
  others <- c(others, list(all_trees[[i]]))
}

direct_ATR <-(list(unlist(direct_ATR)))
prune_and_ATR <- (list(unlist(prune_and_ATR)))
check_mono_manually <- list(unlist(check_mono_manually))
others <- list(unlist(others))

write.xlsx(
  list(
    'direct_ATR' = direct_ATR,
    'prune_and_ATR' = prune_and_ATR,
    'check_mono_manuelly' = check_mono_manually,
    'others' = others
  ),
  file = "C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/classification_trees.xlsx"
)