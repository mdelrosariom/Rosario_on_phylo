library(openxlsx)

export_mono <- function(output_mono, tree_name) {
  if (is.null(output_mono)){
    file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/info_about_mono/_NOT_PROBLEM_WMONO", tree_name, ".xlsx", sep = "")
    write.xlsx(
      list(
        'all_monophyletic' = NULL,
        'to_prune' = NULL,
        'ask_ben' = NULL,
        'still_hope' = NULL
      ),
      file = file_name
    )
    
  }else{
  # Extract lists from the output
  all_of_mono_list <- output_mono$all_of_mono
  to_prune_list <- output_mono$to_prune
  lost_case_ask_ben_list <- output_mono$lost_case_ask_ben
  still_hope_list <- output_mono$still_hope
  
  # Combine lists into data frames
  df_all_of_mono <- as.data.frame(do.call(rbind, all_of_mono_list))
  df_to_prune <- as.data.frame(do.call(rbind, to_prune_list))
  df_lost_case_ask_ben <- as.data.frame(do.call(rbind, lost_case_ask_ben_list))
  df_still_hope <- as.data.frame(do.call(rbind, still_hope_list))
  
  # Create the name of the file
  file_name <- paste("C:/Users/mdrmi/OneDrive/Escritorio/info_about_mono/", tree_name, "_monophyletic_info.xlsx", sep = "")
  
  # Export to Excel
  write.xlsx(
    list(
      'all_monophyletic' = df_all_of_mono,
      'to_prune' = df_to_prune,
      'ask_ben' = df_lost_case_ask_ben,
      'still_hope' = df_still_hope
    ),
    file = file_name
  )
}}

