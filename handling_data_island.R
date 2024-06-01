library("openxlsx")
library("dplyr")

preparing_data <- function(archi) {
  general_inf <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/islands_trees_species.xlsx",1)
                           
  df <- general_inf %>% 
    filter(Archipelago == archi) %>% 
    select(Archipelago, Genus, Species, BEAST_dataset_name, Branching_times, Status_Species)
                                                                             
  
  df$ok_species <- paste(df$Genus, df$Species, sep="_")
  
  df <- df %>% 
    select(Archipelago, ok_species, BEAST_dataset_name, Branching_times, Status_Species) %>%
    rename(archipelago = Archipelago, species = ok_species, tree = BEAST_dataset_name, colonization = Branching_times, origin = Status_Species )
  
  return(df)
}


