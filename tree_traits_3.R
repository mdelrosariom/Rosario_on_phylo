# library("readxl")
# library("writexl")
# library("stringr")
# 
# get_traits_tree <- function(tree, name_of_tree) { 
#   # Import the big dataset that contains the data of all birds
#   species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified_good_version.xlsx") 
#   
#   # Name of the species
#   species <- tree$tip.label
#   traits_tree <- list()
#   
#   for (i in 1:length(species)) {
#     if (grepl("\\.", species[i])) {
#       
#       modified <- gsub("\\.", "_", species[i])
#       
#       modified <- str_extract(modified, "[^_]*_[^_]*")
#       if (modified %in% species_traits$specie) {
#         current_row <- species_traits[species_traits$specie == modified, , drop = FALSE]
#         traits_tree[[i]] <- current_row
#       } else {
#         current_row <- data.frame(
#           specie = species[i],
#           BLC_mean = NA, 
#           BLC_var = NA, 
#           BLC_sd = NA, 
#           BLN_mean = NA,
#           BLN_var = NA,
#           BLN_sd = NA,
#           BW_mean = NA, 
#           BW_var = NA, 
#           BW_sd = NA,
#           BD_mean = NA, 
#           BD_var = NA, 
#           BD_sd = NA
#         )
#         traits_tree[[i]] <- current_row
#       }
#     } else {
#       if (species[i] %in% species_traits$specie) {
#         current_row <- species_traits[species_traits$specie == species[i], , drop = FALSE]
#         traits_tree[[i]] <- current_row
#       } else {
#         current_row <- data.frame(
#           specie = species[i],
#           BLC_mean = NA, 
#           BLC_var = NA, 
#           BLC_sd = NA, 
#           BLN_mean = NA,
#           BLN_var = NA,
#           BLN_sd = NA,
#           BW_mean = NA, 
#           BW_var = NA, 
#           BW_sd = NA,
#           BD_mean = NA, 
#           BD_var = NA, 
#           BD_sd = NA
#         )
#         traits_tree[[i]] <- current_row
#       }
#     }
#   }
#   
#   tree_name <- sub('\\.tree$', '', name_of_tree)
#   file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/", "trait_", tree_name, ".xlsx")
#   
#   # Combine the list of data frames into a single data frame
#   traits_tree <- do.call(rbind, traits_tree)
#   
#   # Write the data frame to an Excel file
#   write_xlsx(traits_tree, file_name)
#   
#   return(traits_tree)
# }
# library("readxl")
# library("writexl")
# library("stringr")
# 
# get_traits_tree <- function(tree, name_of_tree) { 
#   # Import the big dataset that contains the data of all birds
#   species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified_good_version.xlsx") 
#   
#   # Name of the species
#   species <- tree$tip.label
#   traits_tree <- list()
#   
#   for (i in 1:length(species)) {
#     if (grepl("\\.", species[i])) {
#       
#       modified <- gsub("\\.", "_", species[i])
#       
#       modified <- str_extract(modified, "[^_]*_[^_]*")
#       if (modified %in% species_traits$specie) {
#         current_row <- species_traits[species_traits$specie == modified, , drop = FALSE]
#         traits_tree[[i]] <- current_row
#       } else {
#         current_row <- data.frame(
#           specie = species[i], # Use original species name
#           BLC_mean = NA, 
#           BLC_var = NA, 
#           BLC_sd = NA, 
#           BLN_mean = NA,
#           BLN_var = NA,
#           BLN_sd = NA,
#           BW_mean = NA, 
#           BW_var = NA, 
#           BW_sd = NA,
#           BD_mean = NA, 
#           BD_var = NA, 
#           BD_sd = NA
#         )
#         traits_tree[[i]] <- current_row
#       }
#     } else {
#       if (species[i] %in% species_traits$specie) {
#         current_row <- species_traits[species_traits$specie == species[i], , drop = FALSE]
#         traits_tree[[i]] <- current_row
#       } else {
#         current_row <- data.frame(
#           specie = species[i],
#           BLC_mean = NA, 
#           BLC_var = NA, 
#           BLC_sd = NA, 
#           BLN_mean = NA,
#           BLN_var = NA,
#           BLN_sd = NA,
#           BW_mean = NA, 
#           BW_var = NA, 
#           BW_sd = NA,
#           BD_mean = NA, 
#           BD_var = NA, 
#           BD_sd = NA
#         )
#         traits_tree[[i]] <- current_row
#       }
#     }
#   }
#   
#   tree_name <- sub('\\.tree$', '', name_of_tree)
#   file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/", "trait_", tree_name, ".xlsx")
#   
#   # Combine the list of data frames into a single data frame
#   traits_tree <- do.call(rbind, traits_tree)
#   
#   # Write the data frame to an Excel file
#   write_xlsx(traits_tree, file_name)
#   
#   return(traits_tree)
# }
# 
# library("readxl")
# library("writexl")
# library("stringr")
# 
# get_traits_tree <- function(tree, name_of_tree) { 
#   # Import the big dataset that contains the data of all birds
#   species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified_good_version.xlsx") 
#   
#   # Name of the species
#   species <- tree$tip.label
#   traits_tree <- list()
#   
#   for (i in 1:length(species)) {
#     if (species[i] %in% species_traits$specie) {
#       current_row <- species_traits[species_traits$specie == species[i], , drop = FALSE]
#     } else {
#       current_row <- data.frame(
#         specie = species[i],
#         BLC_mean = NA, 
#         BLC_var = NA, 
#         BLC_sd = NA, 
#         BLN_mean = NA,
#         BLN_var = NA,
#         BLN_sd = NA,
#         BW_mean = NA, 
#         BW_var = NA, 
#         BW_sd = NA,
#         BD_mean = NA, 
#         BD_var = NA, 
#         BD_sd = NA
#       )
#     }
#     traits_tree[[i]] <- current_row
#   }
#   
#   tree_name <- sub('\\.tree$', '', name_of_tree)
#   file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/", "trait_", tree_name, ".xlsx")
#   
#   # Combine the list of data frames into a single data frame
#   traits_tree <- do.call(rbind, traits_tree)
#   
#   # Write the data frame to an Excel file
#   write_xlsx(traits_tree, file_name)
#   
#   return(traits_tree)
# }

library("readxl")
library("writexl")
library("stringr")

get_traits_tree <- function(tree, name_of_tree) { 
  # Import the big dataset that contains the data of all birds
  species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified_good_version.xlsx") 
  
  # Name of the species
  species <- tree$tip.label
  traits_tree <- list()
  
  for (i in 1:length(species)) {
    # Adjust species name for lookup
    species_name_lookup <- gsub("\\.", "_", species[i])
    
    if (species_name_lookup %in% species_traits$specie) {
      current_row <- species_traits[species_traits$specie == species_name_lookup, , drop = FALSE]
    } else {
      current_row <- data.frame(
        specie = species[i],
        BLC_mean = NA, 
        BLC_var = NA, 
        BLC_sd = NA, 
        BLN_mean = NA,
        BLN_var = NA,
        BLN_sd = NA,
        BW_mean = NA, 
        BW_var = NA, 
        BW_sd = NA,
        BD_mean = NA, 
        BD_var = NA, 
        BD_sd = NA
      )
    }
    # Store species name with periods for output dataframe
    current_row$specie <- species[i]
    traits_tree[[i]] <- current_row
  }
  
  tree_name <- sub('\\.tree$', '', name_of_tree)
  file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/tree_trait/", "trait_", tree_name, ".xlsx")
  
  # Combine the list of data frames into a single data frame
  traits_tree <- do.call(rbind, traits_tree)
  
  # Write the data frame to an Excel file
  write_xlsx(traits_tree, file_name)
  
  return(traits_tree)
}



