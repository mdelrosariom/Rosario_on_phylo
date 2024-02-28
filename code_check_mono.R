library("phytools")
setwd("C:/Users/mdrmi/Downloads/Phylogenetic trees used for global analysis of island birds/Phylogenetic trees used for global analysis of island birds/Maximum clade credibility trees/")
acrocephalus_tree<-read.nexus("Acrocephalus_maxcred.tre")

birds <- acrocephalus_tree$tip.label
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


for (i in 1:length(all_of_mono)) {
  label_parts <- strsplit(all_of_mono[[i]], "_")
  print(label_parts[[1]][[3]])
  vect <- all_of_mono[grepl(label_parts[[1]][[3]],all_of_mono)]
}












