matrix_to_numeric <- function(x) {
       if (!is.null(x) && !is.na(x)) {
             as.numeric(unlist(x))
         } else {
               NA
           }
   }