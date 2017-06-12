### analysis of plant communities inside and outside of cusion plants
### data from 30 cushion plant in dry meadow tundra

# load community composition data
dat = read.csv("Comp_RA.csv") 

#check data
head(dat)
dim(dat)

# isolate composition data
comp = dat[,4:38]
head(comp)

## plot code
plot= dat$subplot


# load library for analysis
library(vegan)
library(BiodiversityR)

##
facDIS = vegdist(comp, method="bray")

##Plot NMDS
NMDS = metaMDS(comp, distance = "bray")
#plot(NMDS, display = c("sites"))
#plotting script#
plot2 = ordiplot(NMDS, choices=c(1,2), type="none", display="sites")
env.frame <- data.frame(dat$Treat2)
ordisymbol(plot2, env.frame, col=1, rainbow=F, legend=T)

NMDSscores = as.data.frame(scores(NMDS, display = c("sites")))
head(NMDSscores)


# create another frame that excludes cushion plants
cushionComp <- within(comp, rm("TRIDAS", "TRINAN", "SILENE", "MINOBT", "PARPUL")) 
head(cushionComp)

## plot code 
plot= dat$subplot 

cushDIS = vegdist(cushionComp, method = "bray")

cushNMDS = metaMDS(cushionComp, distance = "bray")

cushPlot = ordiplot(cushNMDS, choices=c(1,2), type="none", display="sites")
ordisymbol(cushPlot, env.frame, col=1, rainbow=F, legend=T)

