library("phytools")
#Corvus ruficollis
corvus_tree <- read.nexus("Corvus_maxcred.tre") 

corvus_tree$tip.label

node <-findMRCA(corvus_tree, tips=c( "Corvus_hawaiiensis_HAWAII_AY005928",                       
"Corvus_hawaiiensis_HAWAII_KP161619",                       
"Corvus_hawaiiensis_HAWAII_KP161620"))
subset <- extract.clade(corvus_tree,node)
plotTree(subset)
