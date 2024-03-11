library(openxlsx)

export_mono_4 <- function(output_tree_check_mono, tree_name) {
  
  a <- output_tree_check_mono$all_of_mono
  if (!is.null(unlist(a))) {
    pad <- max(sapply(a, length))
    
    for (i in 1:length(a)) {
      length(a[[i]]) <- pad
    }
    a <- data.frame(t(do.call(rbind, a)), stringsAsFactors = FALSE)
  } else {
    a <- list()
  }
  
  b <- output_tree_check_mono$to_prune
  if (!is.null(unlist(b))) {
    pad1 <- max(sapply(b, length))
    
    for (j in 1:length(b)) {
      length(b[[j]]) <- pad1
    }
    b <- data.frame(t(do.call(rbind, b)), stringsAsFactors = FALSE)
  } else {
    b <- list()
  }
  
  c <- output_tree_check_mono$lost_case_ask_ben
  if (!is.null(unlist(c))) {
    pad2 <- max(sapply(c, length))
    for (k in 1:length(c)) {
      length(c[[k]]) <- pad2
    }
    c <- data.frame(t(do.call(rbind, c)), stringsAsFactors = FALSE)
  } else {
    c <- list()
  }
  
  d <- output_tree_check_mono$still_hope
  if (!is.null(unlist(d))) {
    pad3 <- max(sapply(d, length))
    for (l in 1:length(d)) {
      length(d[[l]]) <- pad3
    }
    d <- data.frame(t(do.call(rbind, d)), stringsAsFactors = FALSE)
  } else {
    d <- list()
  }
  
  file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/info_about_mono_2/", tree_name, "_monophyletic_info_2.xlsx", sep = "")
  
  # Export to Excel
  write.xlsx(
    list(
      'all_monophyletic' = a,
      'to_prune' = b,
      'ask_ben' = c,
      'still_hope' = d
    ),
    file = file_name
  )
}


