library("phytools")
#Corvus ruficollis
tree <- read.nexus("Acrocephalus_maxcred.tre") 

"Acrocephalus_maxcred.tre"$tip.label

node <-findMRCA(tree, tips=c("Acrocephalus_aedon_AF094623",	"Acrocephalus_aedon_AJ004778",	"Acrocephalus_aedon_FJ883020",	"Acrocephalus_aedon_HQ608854",	"Acrocephalus_aedon_AF094623",	"Acrocephalus_aedon_AJ004778",	"Acrocephalus_aedon_FJ883020"
))
subset <- extract.clade(tree,node)
plotTree(subset)

node <-findMRCA(tree, tips=c("Acrocephalus_avicenniae_AJ004237",	"Acrocephalus_avicenniae_AJ004238",	"Acrocephalus_avicenniae_KF547903",	"Acrocephalus_avicenniae_AJ004237",	"Acrocephalus_avicenniae_AJ004238",	"Acrocephalus_avicenniae_KF547903",	"Acrocephalus_avicenniae_AJ004237"
))
subset <- extract.clade(tree,node)
plotTree(subset)

node <-findMRCA(tree, tips=c("Acrocephalus_orientalis_AB159180",	"Acrocephalus_orientalis_AF129464",	"Acrocephalus_orientalis_AJ004289",	"Acrocephalus_orientalis_AJ004785",	"Acrocephalus_orientalis_China_JN574441",	"Acrocephalus_orientalis_FJ883034",	"Acrocephalus_orientalis_HQ608853"
))
subset <- extract.clade(tree,node)
plotTree(subset)


node <-findMRCA(tree, tips=c("Acrocephalus_vaughani_AJ004308",	"Acrocephalus_vaughani_HQ844379",	"Acrocephalus_vaughani_HQ844380",	"Acrocephalus_vaughani_AJ004308",	"Acrocephalus_vaughani_HQ844379",	"Acrocephalus_vaughani_HQ844380",	"Acrocephalus_vaughani_AJ004308"
))
subset <- extract.clade(tree,node)
plotTree(subset)





