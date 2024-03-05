library("readxl")
get_traits_tree <- function(tree){ 
  tree_traits <- list()
  species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified.xlsx") 
  species <- tree$tip.label
  traits_tree <- data.frame()  # Initialize outside the loop
  
  get_traits_tree <- function(tree){ 
    species_traits <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified.xlsx") 
    species <- tree$tip.label
    traits_tree <- list()
    
    for (i in 1:length(species)) {
      if (species[i] %in% species_traits$specie) {
        current_row <- species_traits[species_traits$specie == species[i], , drop = FALSE]
        print(current_row)
        traits_tree <- rbind(traits_tree, current_row)
        
      } else {
        current_row <- data.frame(
          specie_sep = species[[i]],
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
          BD_sd = NA, 
          specie = species[[i]]
        )
        
        traits_tree <- rbind(traits_tree, current_row)
      }
    }
    
    return(traits_tree)
  }
  
    }
  }
  
  # Return the result outside the loop
  return(traits_tree)
}
