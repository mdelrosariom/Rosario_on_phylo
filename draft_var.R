library("openxlsx") 
library("umx") # to pad
library("matrixStats") #var rows

library(ggplot2)

#REUNION 

sp1 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Coracina_newtoniatr_species.xlsx")
sp2 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Fregilupus_variusatr_species.xlsx")
sp3 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Hypsipetes_borbonicusatr_species.xlsx")
sp4 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Mascarinus_mascarinusatr_species.xlsx")
sp5 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Psittacula_equesatr_species.xlsx")
sp6 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Saxicola_tectesatr_species.xlsx")
sp7 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Terpsiphone_bourbonnensisatr_species.xlsx")
sp8 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Zosterops_olivaceusatr_species.xlsx")


# max is sp2, so we need to pad everything with that longuitud 

sp1_blc <- umx_pad(sp1$BLC_mean,58 )
sp3_BLC <- umx_pad(sp3$BLC_mean,58 )
sp4_BLC <- umx_pad(sp4$BLC_mean,58 )
sp5_BLC <- umx_pad(sp5$BLC_mean,58 )
sp6_BLC <- umx_pad(sp6$BLC_mean,58 )
sp7_BLC <- umx_pad(sp7$BLC_mean,58 )
sp8_BLC <- umx_pad(sp8$BLC_mean,58 )
sp2_BLC <-  sp2$BLC_mean
rodrigues_BLC <- cbind(sp1_blc,sp2_BLC,sp3_BLC,sp4_BLC,sp5_BLC,sp6_BLC,sp7_BLC, sp8_BLC)

var_blc_rodrigues <- rowVars(rodrigues_BLC, na.rm=TRUE)


var_blc_rodrigues[is.na(var_blc_rodrigues)] <- 0

graph_blc_ro <- rev(var_blc_rodrigues)
time <- sp2$theoretical_times
data <- data.frame(time,graph_blc_ro)

ggplot(data, aes(x=time, y=graph_blc_ro)) +
  geom_line()


#------------------------MAURI


sp1 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Coracina_newtoniatr_species.xlsx")
sp2 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Fregilupus_variusatr_species.xlsx")
sp3 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Hypsipetes_borbonicusatr_species.xlsx")
sp4 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Mascarinus_mascarinusatr_species.xlsx")
sp5 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Psittacula_equesatr_species.xlsx")
sp6 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Saxicola_tectesatr_species.xlsx")
sp7 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Terpsiphone_bourbonnensisatr_species.xlsx")
sp8 <- read.xlsx("C:/Users/mdrmi/OneDrive/Escritorio/Zosterops_olivaceusatr_species.xlsx")


# max is sp2, so we need to pad everything with that longuitud 

sp1_blc <- umx_pad(sp1$BLC_mean,58 )
sp3_BLC <- umx_pad(sp3$BLC_mean,58 )
sp4_BLC <- umx_pad(sp4$BLC_mean,58 )
sp5_BLC <- umx_pad(sp5$BLC_mean,58 )
sp6_BLC <- umx_pad(sp6$BLC_mean,58 )
sp7_BLC <- umx_pad(sp7$BLC_mean,58 )
sp8_BLC <- umx_pad(sp8$BLC_mean,58 )
sp2_BLC <-  sp2$BLC_mean
rodrigues_BLC <- cbind(sp1_blc,sp2_BLC,sp3_BLC,sp4_BLC,sp5_BLC,sp6_BLC,sp7_BLC, sp8_BLC)

var_blc_rodrigues <- rowVars(rodrigues_BLC, na.rm=TRUE)


var_blc_rodrigues[is.na(var_blc_rodrigues)] <- 0

graph_blc_ro <- rev(var_blc_rodrigues)
time <- sp2$theoretical_times
data <- data.frame(time,graph_blc_ro)

ggplot(data, aes(x=time, y=graph_blc_ro)) +
  geom_line()


#-----------------------BW


sp1_bw <- umx_pad(sp1$BW_mean,58 )
sp3_BW <- umx_pad(sp3$BW_mean,58 )
sp4_BW <- umx_pad(sp4$BW_mean,58 )
sp5_BW <- umx_pad(sp5$BW_mean,58 )
sp6_BW <- umx_pad(sp6$BW_mean,58 )
sp7_BW <- umx_pad(sp7$BW_mean,58 )
sp8_BW <- umx_pad(sp8$BW_mean,58 )
sp2_BW <-  sp2$BW_mean
rodrigues_BW <- cbind(sp1_bw,sp2_BW,sp3_BW,sp4_BW,sp5_BW,sp6_BW,sp7_BW, sp8_BW)

var_BW_rodrigues <- rowVars(rodrigues_BW, na.rm=TRUE)


var_BW_rodrigues[is.na(var_BW_rodrigues)] <- 0

graph_BW_ro <- rev(var_BW_rodrigues)
time <- sp2$theoretical_times
data <- data.frame(time,graph_BW_ro)

ggplot(data, aes(x=time, y=graph_BW_ro)) +
  geom_line()


#------------------BD-------------------------------------------

sp1_BD <- umx_pad(sp1$BD_mean,58 )
sp3_BD <- umx_pad(sp3$BD_mean,58 )
sp4_BD <- umx_pad(sp4$BD_mean,58 )
sp5_BD <- umx_pad(sp5$BD_mean,58 )
sp6_BD <- umx_pad(sp6$BD_mean,58 )
sp7_BD <- umx_pad(sp7$BD_mean,58 )
sp8_BD <- umx_pad(sp8$BD_mean,58 )
sp2_BD <-  sp2$BD_mean
rodrigues_BD <- cbind(sp1_BD,sp2_BD,sp3_BD,sp4_BD,sp5_BD,sp6_BD,sp7_BD, sp8_BD)

var_BD_rodrigues <- rowVars(rodrigues_BD, na.rm=TRUE)


var_BD_rodrigues[is.na(var_BD_rodrigues)] <- 0

graph_BD_ro <- rev(var_BD_rodrigues)
time <- sp2$theoretical_times
data <- data.frame(time,graph_BD_ro)

ggplot(data, aes(x=time, y=graph_BD_ro)) +
  geom_line()






