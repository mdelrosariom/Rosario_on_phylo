



variance_island <- function(island_name, all_fixed) {
  info_island <- preparing_data(island_name) 
  trees_island <- unique(info_island$tree)
  
  for (i in trees_island) {
    for (j in all_fixed) {
      if (grepl( i, j)) {
        info_island_tree <- info_island[info_island$tree==i,]
        
        tree <- read.nexus(all_fixed[j])
        traits <- read.xlsx(paste("C:/Users/mdrmi/OneDrive/Escritorio/checked_traits_trees/trait_", all_fixed[j], ".xlsx", sep = ""))
        traits_atr <- traits %>% select(c('specie', 'BLC_mean', 'BLN_mean', 'BW_mean', 'BD_mean'))
        rownames(traits_atr) <- traits_atr$specie
        traits_atr <- traits_atr[, -1]
        
        try({
          fit1 <- mvBM(tree, traits_atr, model = "BM1", method = "rpf")
          complete_data <- estim(tree, traits_atr, fit1, asr = FALSE)
          ATR <- estim(tree, complete_data$estimates, fit1, asr = TRUE)
          DaAnc <- rbind(traits_atr, ATR$estimates)
          res <- mvbrownian_path(as.matrix(DaAnc), tree, sigma = fit1$sigma, steps = 0.1, stochastic = TRUE, plot = FALSE)
       
          })
        
        # If the try block fails, execute the following
        if (exists("fit1")) {
          fit1 <- mvgls(as.matrix(traits_atr) ~ 1, data = as.data.frame(traits_atr), tree, model = "BM", penalty = "RidgeArch")
          ATR <- ancestral(fit1)
          DaAnc <- rbind(traits_atr, ATR)
          sigma <- fit1$sigma$Pinv
          res <- mvbrownian_path(as.matrix(DaAnc), tree, sigma = sigma, steps = 0.1, stochastic = TRUE, plot = FALSE)
        }
        for (species in 1:length(info_island_tree$species)) {
          times_colo <-as.numeric(unlist(strsplit(info_island_tree$colonization[species], ",")))
          if length(times_colo)==1 and !is.na(times_colo){
          traits_speciestraits_time_species <- function(tree, species, res, times_colo,i )
          }

        }
      }
    }
  }
}