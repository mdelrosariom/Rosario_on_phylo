library("phytools")
library("mvMORPH")
library("phylobase")
library("openxlsx")
library("dplyr")

#functions 
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_3.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/Julien_simmap_cont.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_branch_finder.R")
all_fixed <- list.files(path="C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees"
                        , pattern="\\.tre$", all.files=FALSE,
                        full.names=FALSE)

island_trees <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/islands_trees_species.xlsx")


island_trees$ok_species <- paste(island_trees$Genus, island_trees$Species, sep= " ")


rodrigues_info<-island_trees[island_trees$Archipelago=="Rodrigues",]


rodrigues_info <- rodrigues_info %>% select(BEAST_dataset_name, ok_species)

list_trees_archi <- list()


# find trees in archipelago 
for (i in rodrigues_info$BEAST_dataset_name) {
  found <- FALSE
  for (j in seq_along(all_fixed)) {
    cleaned_j <- sub("_.*", "", all_fixed[[j]]) 
    if (i == cleaned_j) {
      print(i)
      found <- TRUE
      list_trees_archi <- c(list_trees_archi,all_fixed[[j]])
      break  # Exit inner loop once match is found
    }
  }
  if (!found) {
    print(paste(i, "NOT FOUND"))
  }
}

#run ATE for those trees 

list_trees_archi <- c(list_trees_archi,"Foudia_maxcred_rate_of_cytb_2.tre")

list_trees_archi[[1]]

tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/checked_fixedtrees/Acrocephalus_maxcreddd.tre")

traits<-get_traits_tree(tree, list_trees_archi[[1]] )

traits_Acrocephalus <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/ trait_ Acrocephalus_maxcred.tre .xlsx")


traits_atr_Acrocephalus <- traits %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr_Acrocephalus) <- traits_atr_Acrocephalus$specie
traits_atr_Acrocephalus <- traits_atr_Acrocephalus[, -1]


fit1<-mvBM(tree, traits_atr_Acrocephalus,  model="BM1", method="rpf")
#ATR <- estim(tree,traits, fit1, asr=TRUE)
#simulate data in NAs

complete_data_Acrocephalus <- estim(tree, traits_atr_Acrocephalus, fit1, asr=FALSE)
#running ate
ATR <- estim(tree, complete_data_Acrocephalus$estimates , fit1, asr=TRUE)

DaAnc <- rbind(complete_data_Acrocephalus$estimates, ATR$estimates)

res_Acrocephalus <- mvbrownian_path(DaAnc, tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)


#NOW WE NEED TO SEE THE EVOLUTIONARY PATH OF THE SPECIES, THAT IN THIS CASE IS ONLY 

#Acrocephalus rodericanus CHECKED IN RODRIGUES_INFO 

tree_path <- as(tree, "phylo4d")

evo_path_specie <- ancestors(tree_path, "Acrocephalus_rodericanus", "ALL")

evo_path_specie <- as.vector(evo_path_specie)
evo_path_specie <- rev(evo_path_specie)

nodes<- cbind(evo_path_specie[2:length(evo_path_specie )],evo_path_specie[1:length(evo_path_specie )-1])

branches <- list()

for (n in 1:nrow(nodes)){
  
  first_node <- nodes[n,1]
  second_node <- nodes[n,2]
  
  branch <- find_branch_number(first_node, second_node, tree$edge)
 
  branches <- c(branches,branch)
  
}

branches <-as.vector(unlist(branches))



#now we make the data of the times and traits for the branches involved with the specie

#first time
times <- list()

for (i in branches){
  print(res_Acrocephalus$times[[i]])
  times <- c(times,unlist(res_Acrocephalus$times[[i]]))
  }
times_species <- as.vector(unlist(times))
times_species <- round(times_species, 6)

#second traits 

traits <- list()

for (i in branches) {
  traits <- c(traits, list(res_Acrocephalus$traits[[i]]))
}

# Combine the subsets into a single dataframe
species_traits <- do.call(rbind, traits)

#now we make a vector that will check the traits each certain time steps. in this case 0.1 time steps (meaning 100.000 years)

sampling_times <- seq(0, max(times_species), by = 0.1)

#as the values of times will not be exactly the same as the sampling time, we need to get the closest of times_species from 
#sampling_times
closest_indices <- sapply(sampling_times, function(x) which.min(abs(times_species - x)))

closest_times <- times_species[closest_indices]

traits_in_time_species <- list()

for (time in closest_times){
  index <- which(times_species == time)
  print(index)
  trait_time <- species_traits[index,]
  print(trait_time)
  traits_in_time_species <- c(traits_in_time_species, list(trait_time))
  }

combined_time_traits <- do.call(rbind, traits_in_time_species)
















