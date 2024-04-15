birds <- c("xx_yy_z", "xx_yy_i", "xx_yy_khnk", "yy_oo_kk", "yy_oo_ll", "yy_pp_xx", "zz_ii_o", "zz_ii_o", "zz_ii_kjn", "zz_ii_ppplb", "zz_ii_ppplb")

subspecies <- list()

label_parts <- strsplit(birds, "_")
sub_geo <- sapply(label_parts, function(x) paste(x[3], sep = "_"))
sub_geo <- unique(sub_geo)
for (i in 1:length(sub_geo)) {
  print(grepl(sub_geo[[i]],birds))
  print(birds[grepl(sub_geo[[i]],birds)])
  vect <- birds[grepl(sub_geo[[i]],birds)]
  print(vect)
}

