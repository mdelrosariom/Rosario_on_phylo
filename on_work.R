library("stringr")

acrocephalus_species <-data.frame(species =acrocephalus_tree$tip.label)

lista <- list(acrocephalus_species)
lista <-as.character(lista)
# Function to extract the common prefix
get_prefix <- function(string) {
  strsplit(string, "_")[[1]][2]
}

# Split the list based on common prefixes
aa<- sapply(acrocephalus_species, get_prefix)
result_list <- saa
plit(lista, sapply(lista, get_prefix))

# Convert the result to a list of lists for better readability
result_list <- lapply(result_list, as.vector)

# Print the result
print(result_list)

result_list<-as.character(result_list)

for (i in 1:length(result_list)) {
  sublist <- result_list[[i]]
  if (length(sublist) > 1) {
    if (is.monophyletic(acrocephalus_tree, sublist)) {
      to_prune <- sublist[2:length(sublist)]
      acrocephalus_tree <- drop.tip(acrocephalus_tree, to_prune)
    }
  }
}



#startsWith(x, prefix)
