classifier <- function(tree,direct_ATR, prune_and_ATR, check_mono_manually, others, no_mono){
  # classification in the lists we mentioned before 
  if (is.null(tree)){
    no_mono <<- c(no_mono, list(all_trees[[i]]))
  }
  else if (length(tree$all_of_mono) == 0 | is.null(as.vector(unlist(tree$all_of_mono)))) { 
    direct_ATR <<- c(direct_ATR, list(all_trees[[i]]))
  } else if ((length(tree$lost_case_ask_ben) == 0 & length(tree$still_hope) == 0) |
             (is.null(as.vector(unlist(tree$lost_case_ask_ben))) & is.null(as.vector(unlist(tree$still_hope))))) {
    prune_and_ATR <<- c(prune_and_ATR, list(all_trees[[i]]))
  } else if (length(tree$lost_case_ask_ben) == 0 | is.null(as.vector(unlist(tree$lost_case_ask_ben)))) { 
    check_mono_manually <<- c(check_mono_manually, list(all_trees[[i]]))
  } else {
    others <<- c(others, list(all_trees[[i]]))
  }
  
  direct_ATR <-(list(unlist(direct_ATR)))
  prune_and_ATR <- (list(unlist(prune_and_ATR)))
  check_mono_manually <- list(unlist(check_mono_manually))
  others <- list(unlist(others))
  

  
}