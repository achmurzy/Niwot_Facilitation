# load community composition data
dat = read.csv("Comp_RA.csv") 

#check data
head(dat)
dim(dat)

# isolate composition data
comp = dat[,3:37]
head(comp)

# create another frame that excludes cushion plants
cushionComp <- within(comp, rm("TRIDAS", "TRINAN", "SILENE", "MINOBT", "PARPUL"))

## plot code
plot= dat$subplot

#modify the treatment colum

tundraTreat <- sapply(dat$Treat, function(x) if (x == 'N' || x == 'S') 'T' else 'C')
tundraTreat <- factor(tundraTreat)

# load library for analysis
library(vegan)
library(BiodiversityR)

cushDIS = vegdist(cushionComp, method = "bray")
cushANOVA = adonis(cushDIS ~ Treat, data=dat)
cushTUNDRA = adonis(cushDIS ~ tundraTreat)
cushNMDS = metaMDS(cushionComp, distance = "bray")

png(filename="Cushion_Ordination")
cushPlot = ordiplot(cushNMDS, choices=c(1,2), type="none", display="sites")
ordisymbol(cushPlot, env.frame, col=1, rainbow=F, legend=T)
dev.off()
