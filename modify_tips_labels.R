library("ape")

orig_tiplabels <- c("Alice", "Bob", "Cindy")
o_tree <- rtree(n = 3, tip.label = orig_tiplabels)
#plot(orig_tree)

new_tiplabels <- c("Debbie_xgg_gg", "Elrond_y_x", "Frank_j_k")
o_tree$tip.label <- new_tiplabels
#plot(orig_tree)


for (i in 1:length(o_tree$tip.label)){
  x = o_tree$tip.label[i]
  o_tree$tip.label[i]<-stringr::str_extract(x, "[^_]*_[^_]*")}
  #print(orig_tree)


plot(o_tree)

