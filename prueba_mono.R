list_to_test_monophyleny <-c()

for (i in 1:length(common_prefix)) {
  list <- c()
  for (j in 1:length(acrocephalus_tree$tip.label)) {
    if (grepl(common_prefix[[i]], acrocephalus_tree$tip.label[[j]], fixed = TRUE)) {
      list <- c(list, acrocephalus_tree$tip.label[[j]])
    }
  }
  if len(list)
}











