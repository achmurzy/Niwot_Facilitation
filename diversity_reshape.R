library(reshape)
library(gdata)
library(vegan)

data <- read.xls("Niwot_Fac_Data_20160802.xlsx", 1)
melted <- melt(data, id = c("PLOT", "SUBPLOT", "SPECIES"), measured = c("COUNT"))

#Beware duplicate data when casting rows
casted <- cast(melted, PLOT + SUBPLOT ~ SPECIES, fill = 0)

#We will probably want a better way to handle this so that we can
#use information in the PLOT column to make the ordination graph more readable
casted$PLOT <- NULL
casted$SUBPLOT <- NULL

ord <- metaMDS(casted, autotransform=FALSE)
plot(ord)
