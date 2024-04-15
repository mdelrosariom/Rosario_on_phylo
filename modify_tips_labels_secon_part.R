library("readxl")

avonet_raw <- read_excel("C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified.xlsx") 

names <- avonet_raw$specie
label_parts <- strsplit(names, " ") # separate the names by the subscore

# Combine the first and second parts with an underscore
modified_names <- sapply(label_parts, function(parts) paste(parts[1], parts[2], sep = "_"))

avonet_raw$modified_names <- modified_names

write.xlsx(avonet_raw, "C:/Users/mdrmi/OneDrive/Escritorio/codes_phylo/traits_purified.xlsx")
