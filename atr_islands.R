library("phytools")
library("mvMORPH")
library("phylobase")
library("openxlsx")
library("dplyr")

#functions 
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_traits_3.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/Julien_simmap_cont.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/tree_branch_finder.R")
source("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_on_time_species.R")

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



tree <- read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/checked_fixedtrees/Acrocephalus_maxcreddd.tre")

traits<-get_traits_tree(tree, "Acrocephalus_maxcreddd.tre" )

traits_Acrocephalus <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_ Acrocephalus_maxcred.tre .xlsx")


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

nodes<- cbind(evo_path_specie[1:length(evo_path_specie )-1],evo_path_specie[2:length(evo_path_specie )])

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
combined_time_traits <- as.data.frame(combined_time_traits)


time_traits_Acrocephalus_rodericanus <-data.frame("BLC_mean" = combined_time_traits$BLC_mean, 
                                             "BLN_mean" = combined_time_traits$BLN_mean, 
                                             "BW_mean" = combined_time_traits$BW_mean,
                                             "BD_mean" = combined_time_traits$BD_mean,
                                             "time_tree" = closest_times,
                                             "theoretical_times" = sampling_times )



write.xlsx(time_traits_Acrocephalus_rodericanus, "C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees/Acrocephalus_rodericanus_traits_time.xlsx")

#-------------------------------------------------------------------------------
#second tree in Rodrigues island
#"Columbiformes_maxcred.tre"

tree<-read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees/Columbiformes_maxcred.tre")
traits<-get_traits_tree(tree, list_trees_archi[[2]] )

traits_Columbiformes <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/ trait_ Columbiformes_maxcred.tre .xlsx")

traits_atr_Columbiformes  <- traits_Columbiformes %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr_Columbiformes ) <- traits_atr_Columbiformes$specie
traits_atr_Columbiformes <- traits_atr_Columbiformes[, -1]


fit1<-mvBM(tree, traits_atr_Columbiformes,  model="BM1", method="rpf")
#ATR <- estim(tree,traits, fit1, asr=TRUE)
#simulate data in NAs

complete_data_Columbiformes <- estim(tree, traits_atr_Columbiformes, fit1, asr=FALSE)
#running ate
ATR <- estim(tree, complete_data_Columbiformes$estimates , fit1, asr=TRUE)

DaAnc <- rbind(complete_data_Columbiformes$estimates, ATR$estimates)

res_Columbiformes <- mvbrownian_path(DaAnc, tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)


#NOW WE NEED TO SEE THE EVOLUTIONARY PATH OF THE SPECIES, THAT IN THIS CASE IS ONLY 

#Acrocephalus rodericanus CHECKED IN RODRIGUES_INFO 


data_time_traits_Columbiformes <- traits_on_time_species(tree, "Pezophaps_solitaria",res_Columbiformes)



tree_path <- as(tree, "phylo4d")

evo_path_specie <- ancestors(tree_path, "Pezophaps_solitaria", "ALL")

evo_path_specie <- as.vector(evo_path_specie)
evo_path_specie <- rev(evo_path_specie)

nodes<-  cbind(evo_path_specie[1:length(evo_path_specie )-1], evo_path_specie[2:length(evo_path_specie )])

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
  
  times <- c(times,unlist(res_Columbiformes$times[[i]]))
}
times_species <- as.vector(unlist(times))
times_species <- round(times_species, 6)

#second traits 

traits <- list()

for (i in branches) {
  traits <- c(traits, list(res_Columbiformes$traits[[i]]))
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
combined_time_traits <- as.data.frame(combined_time_traits)
time_traits_Pezophaps_solitaria <-data.frame("BLC_mean" = combined_time_traits$BLC_mean, 
                                             "BLN_mean" = combined_time_traits$BLN_mean, 
                                             "BW_mean" = combined_time_traits$BW_mean,
                                             "BD_mean" = combined_time_traits$BD_mean,
                                             "time_tree" = closest_times,
                                             "theoretical_times" = sampling_times )
                                             
write.xlsx(time_traits_Pezophaps_solitaria,"C:/Users/mdrmi/OneDrive/Escritorio/time_traits_Pezophaps_solitaria.xlsx")


#-------------------------------------------------------------------------------


tree<-read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees/Psittaciformes_maxcred.tre")
traits<-get_traits_tree(tree, list_trees_archi[[3]] )

traits_Psittaciformes <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/ trait_ Psittaciformes_maxcred.tre .xlsx")

traits_atr_Psittaciformes  <- traits_Psittaciformes %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr_Psittaciformes ) <- traits_atr_Psittaciformes$specie
traits_atr_Psittaciformes <- traits_atr_Psittaciformes[, -1]


fit1<-mvBM(tree, traits_atr_Psittaciformes,  model="BM1", method="rpf")
#ATR <- estim(tree,traits, fit1, asr=TRUE)
#simulate data in NAs

complete_data_Psittaciformes <- estim(tree, traits_atr_Psittaciformes, fit1, asr=FALSE)
#running ate
ATR <- estim(tree, complete_data_Psittaciformes$estimates , fit1, asr=TRUE)

DaAnc <- rbind(complete_data_Psittaciformes$estimates, ATR$estimates)

res_Psittaciformes <- mvbrownian_path(DaAnc, tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)


#NOW WE NEED TO SEE THE EVOLUTIONARY PATH OF THE SPECIES, THAT IN THIS CASE IS ONLY 

#Acrocephalus rodericanus CHECKED IN RODRIGUES_INFO 

tree_path <- as(tree, "phylo4d")

evo_path_specie <- ancestors(tree_path, "Psittacula_exsul", "ALL")

evo_path_specie <- as.vector(evo_path_specie)
evo_path_specie <- rev(evo_path_specie)

nodes<-  cbind(evo_path_specie[1:length(evo_path_specie )-1], evo_path_specie[2:length(evo_path_specie )])

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
  
  times <- c(times,unlist(res_Psittaciformes$times[[i]]))
}
times_species <- as.vector(unlist(times))
times_species <- round(times_species, 6)

#second traits 

traits <- list()

for (i in branches) {
  traits <- c(traits, list(res_Psittaciformes$traits[[i]]))
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
combined_time_traits <- as.data.frame(combined_time_traits)
time_traits_Pezophaps_solitaria <-data.frame("BLC_mean" = combined_time_traits$BLC_mean, 
                                             "BLN_mean" = combined_time_traits$BLN_mean, 
                                             "BW_mean" = combined_time_traits$BW_mean,
                                             "BD_mean" = combined_time_traits$BD_mean,
                                             "time_tree" = closest_times,
                                             "theoretical_times" = sampling_times )

write.xlsx(time_traits_Pezophaps_solitaria,"C:/Users/mdrmi/OneDrive/Escritorio/time_traits_Psittacula_exsul.xlsx")

#-------------------------------------------------------------------------------

tree<-read.nexus("C:/Users/mdrmi/OneDrive/Escritorio/all_fixed_trees/Foudia_maxcred_rate_of_cytb_2.tre")
traits<-get_traits_tree(tree, list_trees_archi[[4]] )

traits_Foudia <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/ trait_ Foudia_maxcred_rate_of_cytb_2.tre .xlsx")

traits_atr_Foudia  <- traits_Foudia %>% select(c('specie','BLC_mean','BLN_mean', 'BW_mean', 'BD_mean'))
#fixes the structure of traits
rownames(traits_atr_Foudia ) <- traits_atr_Foudia$specie
traits_atr_Foudia <- traits_atr_Foudia[, -1]


fit1<-mvBM(tree, traits_atr_Foudia,  model="BM1", method="rpf")
#ATR <- estim(tree,traits, fit1, asr=TRUE)
#simulate data in NAs

complete_data_Foudia <- estim(tree, traits_atr_Foudia, fit1, asr=FALSE)
#running ate
ATR <- estim(tree, complete_data_Foudia$estimates , fit1, asr=TRUE)

DaAnc <- rbind(complete_data_Foudia$estimates, ATR$estimates)

res_Foudia <- mvbrownian_path(DaAnc, tree, sigma=fit1$sigma, steps=0.1, stochastic=TRUE, plot=FALSE)


#NOW WE NEED TO SEE THE EVOLUTIONARY PATH OF THE SPECIES, THAT IN THIS CASE IS ONLY 

#Acrocephalus rodericanus CHECKED IN RODRIGUES_INFO 


traits_on_time_species(tree,"Foudia_flavicans",res_Foudia)


tree_path <- as(tree, "phylo4d")

evo_path_specie <- ancestors(tree_path, "Foudia_flavicans", "ALL")

evo_path_specie <- as.vector(evo_path_specie)
evo_path_specie <- rev(evo_path_specie)

nodes<-  cbind(evo_path_specie[1:length(evo_path_specie )-1], evo_path_specie[2:length(evo_path_specie )])

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
  
  times <- c(times,unlist(res_Foudia$times[[i]]))
}
times_species <- as.vector(unlist(times))
times_species <- round(times_species, 6)

#second traits 

traits <- list()

for (i in branches) {
  traits <- c(traits, list(res_Foudia$traits[[i]]))
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
combined_time_traits <- as.data.frame(combined_time_traits)
time_traits_Foudia_flavicans <-data.frame("BLC_mean" = combined_time_traits$BLC_mean, 
                                             "BLN_mean" = combined_time_traits$BLN_mean, 
                                             "BW_mean" = combined_time_traits$BW_mean,
                                             "BD_mean" = combined_time_traits$BD_mean,
                                             "time_tree" = closest_times,
                                             "theoretical_times" = sampling_times )

write.xlsx(time_traits_Foudia_flavicans,"C:/Users/mdrmi/OneDrive/Escritorio/time_traits_Foudia_flavicans.xlsx")


################################################################################






