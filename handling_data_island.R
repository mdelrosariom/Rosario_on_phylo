library("openxlsx")
library("dplyr")

preparing_data <- function(archi) {
  general_inf <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/islands_trees_species.xlsx")
  
  df <- general_inf %>% 
    filter(Archipelago == archi) %>% 
    select(Archipelago, Genus, Species, BEAST_dataset_name, Branching_times)
  
  df$ok_species <- paste(df$Genus, df$Species, sep=" ")
  
  df <- df %>% 
    select(Archipelago, ok_species, BEAST_dataset_name, Branching_times) %>%
    rename(archipelago = Archipelago, species = ok_species, tree = BEAST_dataset_name, colonization = Branching_times)
  
  return(df)
}


