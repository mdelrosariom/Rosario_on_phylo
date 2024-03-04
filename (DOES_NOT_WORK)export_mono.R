export_mono <- function(output_mono){
#all this are lists of lists and we need to homogenize size for export
all_of_mono_list <- output_mono$all_of_mono
to_prune_list <- output_mono$to_prune
lost_case_ask_ben_list <- output_mono$lost_case_ask_ben
still_hope_list <- output_mono$still_hope
#we need to put the next chunck for is the list are empty because if not it 
#will not run because list empty so we will give it an element
if (length(all_of_mono_list) == 0){
  all_of_mono_list <- 0
}
if (length(to_prune_list) == 0){
  to_prune_list <- 0
}
if (length(lost_case_ask_ben_list) == 0){
  lost_case_ask_ben_list <- 0
}

if (length(still_hope_list) == 0){
  still_hope_list<- 0
}


#see which list inside the lists of lists is bigger
max_length_ml <- max(sapply(all_of_mono_list, length))
max_length_tp <- max(sapply(to_prune_list, length))
max_length_lcab <- max(sapply(lost_case_ask_ben_list, length))
max_length_sh <- max(sapply(still_hope_list, length))

# Pad shorter lists with NA values
padded_data_ml <- lapply(all_of_mono_list, function(x) c(x, rep(NA, max_length_ml - length(x))))
padded_data_tp <- lapply(to_prune_list, function(x) c(x, rep(NA, max_length_tp - length(x))))
padded_data_lcab <- lapply(lost_case_ask_ben_list, function(x) c(x, rep(NA, max_length_lcab - length(x))))
padded_data_sh <- lapply(still_hope_list, function(x) c(x, rep(NA, max_length_sh - length(x))))

# Convert the list of padded lists to a matrix idk why, but it works so ok. 
matrix_ml <- matrix(unlist(padded_data_ml), nrow = max_length_ml, byrow = TRUE)
matrix_tp <- matrix(unlist(padded_data_tp), nrow = max_length_tp, byrow = TRUE)
matrix_lcab <- matrix(unlist(padded_data_lcab), nrow = max_length_lcab, byrow = TRUE)
matrix_sh <- matrix(unlist(padded_data_sh), nrow = max_length_sh, byrow = TRUE)

# Convert the matrix to a data frame
df_ml <- data.frame(matrix_ml)
df_tp <- data.frame(matrix_tp)
df_lcab <- data.frame(matrix_lcab)
df_sh <- data.frame(matrix_sh)

# for naming the file, we want the name of the tree
label_parts <- strsplit(all_trees[1], "_")
common_prefix <- sapply(label_parts, function(x) paste(x[1], sep = "_"))

# create the name of the file 
name_of_file <- paste("C:/Users/mdrmi/OneDrive/Escritorio/info_about_mono/",common_prefix ,"_monophyletic_info.xlsx", sep = "")

#combine all dataframes and naming the names of the sheet 
dataset_names <- list('all_monophyletic' = df_ml, 'to_prune' = df_tp, 'ask_ben' = df_lcab, 'still_hope' =df_sh)

#export 
write.xlsx(dataset_names, file = name_of_file) }

