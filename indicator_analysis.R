#Indval computes an indicator index for each species according
#to abundance data across a set of sites. The idea is to find the
#species 'characteristic' of a set of sites.

library(gdata)
# load community composition data
dat = read.csv("Comp_RA.csv") 
stems = read.xls("stems.xlsx")

# isolate composition data
comp = dat[,4:38]
comp <- within(comp, rm("SILENE"))

stems <- stems[,-c(1, 37)]
stems <- within(stems, rm("SILENE"))
stems <- apply(stems, c(1,2), function(x) if(is.na(x)) 0 else x)

## Create vector of clustering based on plot code
extractPlot <- function(x) 	
	{ lengthC = nchar(toString(x)) 
	char = substr(x, lengthC, lengthC) 
	if (char == 'N' || char == 'S') 1 else 2 }
treat <- sapply(dat$Treat, extractPlot)

# load library for analysis
library(vegan)
library(BiodiversityR)

## Distance matrix can be used with k-means to generate groups
#facDIS = vegdist(comp, method="bray")
#We already know our groups - cushion (c) and tundra (t)

print("Starting indicator analysis")
library(labdsv)

print("Community-wide clustering")
#indicator <- indval(comp, treat, p=0.01)
#summary(indicator) don't perform on relative abundances

indicator_s <- indval(stems, treat, p=0.01)
summary(indicator_s)

if(FALSE){
tundraComp <- subset(comp, treat == 1) 
cushionComp <- subset(comp, treat == 2)
#Remove species not occuring in cushions
cushionComp <- cushionComp[,colSums(cushionComp != 0) > 0]
#Randomly distribute some number of clusters over the sites
print("Tundra clustering")
clustering <- sample(1:3, nrow(tundraComp), replace=TRUE)
indicator_t <- indval(tundraComp, clustering)
summary(indicator_t)

print("Cushion clustering")
clustering <- sample(1:3, nrow(cushionComp), replace=TRUE)
indicator_c <- indval(cushionComp, clustering)
summary(indicator_c) }

