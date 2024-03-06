check_monophyly <- function(tree_name_file) {
  X_tree <- read.nexus(tree_name_file) 
  birds <- X_tree$tip.label
  
  label_parts <- strsplit(birds, "_")
  common_prefix <- sapply(label_parts, function(x) paste(x[1], x[2], sep = "_"))
  
  if (identical(birds, common_prefix)) {
    # We don't want this
  } else {
    common_prefix <- unique(common_prefix)
    
    all_of_sp <- list()
    
    for (j in 1:length(common_prefix)) {
      vect <- birds[grepl(common_prefix[j], birds)]
      all_of_sp <- c(all_of_sp, list(vect))
    }      
    
    all_of_mono <- list()
    
    for (i in 1:length(all_of_sp)) {
      if (length(all_of_sp[[i]]) > 1) {
        
        all_of_mono <- c(all_of_mono, list(all_of_sp[[i]]))
      } 
    }
    
    to_prune <- c()
    to_inspect <- c()
    
    for (i in 1:length(all_of_mono)) {
      if (is.monophyletic(X_tree, all_of_mono[[i]])) {
        to_prune <- c(to_prune, list(all_of_mono[[i]]))
      } else {
        to_inspect <- c(to_inspect, list(all_of_mono[[i]]))
      }
    }
    
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
    
    return(list(
      all_of_mono = all_of_mono,
      to_prune = to_prune,    
      lost_case_ask_ben = lost_case_ask_ben,
      still_hope = still_hope
    ))
  }
}

