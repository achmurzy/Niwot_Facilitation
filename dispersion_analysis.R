#we're going to compute functional dispersion because forget you
##Calculate FD
library(FD)

cushion = read.csv("Traits_Cushion.csv")
tundra = read.csv("Traits_Tundra.csv")

#Get abundances for species with trait data
abundances=read.csv("Comp_RA.csv")
#Removing rare species
abundances <- within(abundances,
   rm("ALLGEY", "ANDSEP", "DRABA", "HERBWHITE",
                            "KOBMYO", "SAXRHO", "SILENE"))

#Remove rare and cushion plants from cushion data
abundancesCushion <- within(abundances,
   rm("CASOCC", "SEDLAN", "MINOBT", "PARPUL"))
abundancesCushion <-
    abundancesCushion[abundancesCushion$Treat == 'C', ]

#Remove rare plants from tundra data
abundancesTundra <- within(abundances, rm("PHLOX"))
abundancesTundra <-
    abundancesTundra[abundancesTundra$Treat != 'C', ]

#finagling to satisfy FD, vixen that she is
abundancesCushion <- abundancesCushion[, -3]
abundancesTundra <- abundancesTundra[, -3]
abundancesCushion <- abundancesCushion[, -2]
abundancesTundra <- abundancesTundra[, -2]
abundancesCushion <- abundancesCushion[, -1]
abundancesTundra <- abundancesTundra[, -1]

#more finagling - We do not include species names in the
#trait mean files, so add species name from abundance data
rownames(cushion) <- colnames(abundancesCushion)
rownames(tundra) <- colnames(abundancesTundra)

tundraIndicators <- c("MINOBT", "CAMROT", "AREFEN", "CARRUP", "SEDLAN", "GEUROS", "ARTSCO", "TRIDAS", "ERIPIN", "BISBIS", "ERINAN", "PARPUL")
indicatorTundra <- which(rownames(tundra) %in% tundraIndicators) 
indTundra <- tundra[indicatorTundra,]

cushionIndicators <- c("OREALP", "LLOSER", "FESBRA", "HELMOR", "PHLOX")
indicatorCushion <- which(rownames(cushion) %in% cushionIndicators)
indCushion <- cushion[indicatorCushion,]

tundraIndicatorAbundances <- 
	subset(abundancesTundra,select=tundraIndicators)
tundraIndicatorAbundances <- 
	tundraIndicatorAbundances[,order(colnames(tundraIndicatorAbundances))]
cushionIndicatorAbundances <- 
	subset(abundancesCushion, select=cushionIndicators)
cushionIndicatorAbundances <- 
	cushionIndicatorAbundances[,order(colnames(cushionIndicatorAbundances))]

##Check to make sure same number of species in both files CRITICAL##
##Species names must be in same order in both files

#T=number of traits
TC <- dim(cushion)[2]
tc <- dim(cushion)[1]
tc

TT <- dim(tundra)[2]
tt <- dim(tundra)[1]
tt

#c=number of communities
CC<-dim(abundancesCushion)[1]
cc<-dim(abundancesCushion)[2]
cc

CT<-dim(abundancesTundra)[1]
ct<-dim(abundancesTundra)[2]
ct

print(dim(abundancesCushion)[2])
print(dim(cushion[1]))
print(dim(abundancesTundra)[2])
print(dim(tundra[1]))

#check coherence of number of species in 'traits' and 'abundances'
if(dim(abundancesCushion)[2]!=dim(cushion)[1])stop("error:differentnumberofspeciesin'cushion'and'abundances'matrices")

if(dim(abundancesTundra)[2]!=dim(tundra)[1])stop("error:differentnumberofspeciesin'tundra'and'abundances'matrices")

#Create subsets of trait data

cushionArea <- within(cushion, rm("LDMC", "SLA", "HEIGHT_cm")) 
tundraArea <- within(tundra, rm("LDMC", "SLA", "HEIGHT_cm"))

cushionLDMC <- within(cushion, rm("AREA_cm2", "SLA", "HEIGHT_cm"))
tundraLDMC <- within(tundra, rm("AREA_cm2", "SLA", "HEIGHT_cm"))

cushionSLA <- within(cushion, rm("AREA_cm2", "LDMC", "HEIGHT_cm"))
tundraSLA <- within(tundra, rm("AREA_cm2", "LDMC", "HEIGHT_cm"))

cushionHeight <- within(cushion, rm("AREA_cm2", "LDMC", "SLA"))
tundraHeight <- within(tundra, rm("AREA_cm2", "LDMC", "SLA"))


##RUN FD CODE
# Indicator analysis
if(TRUE)
{
#Tundra indicators
resTund=dbFD(indTundra,tundraIndicatorAbundances,
             stand.FRic= TRUE, corr = c("lingoes"))
write.table(resTund,file="FD_tundra_all-res_ind.csv",
            row.names=TRUE, col.names=NA, sep=",")

#Cushion indicators
resCush=dbFD(indCushion,cushionIndicatorAbundances,
             stand.FRic= TRUE, corr = c("lingoes"))
write.table(resCush,file="FD_cushion_all-res_ind.csv",
            row.names=TRUE,col.names=NA, sep=",")
}

if(FALSE)
{
# Tundra replicates

resTund=dbFD(tundra,abundancesTundra,
             stand.FRic= TRUE, corr = c("lingoes"))
resTundArea=dbFD(tundraArea,abundancesTundra,
             stand.FRic= TRUE, corr = c("lingoes"))
resTundLDMC=dbFD(tundraLDMC,abundancesTundra,
             stand.FRic= TRUE, corr = c("lingoes"))
resTundSLA=dbFD(tundraSLA,abundancesTundra,
             stand.FRic= TRUE, corr = c("lingoes"))
resTundHeight=dbFD(tundraHeight,abundancesTundra,
             stand.FRic= TRUE, corr = c("lingoes"))

##write table with FD metrics
write.table(resTund,file="FD_tundra_all-res.csv",
            row.names=TRUE, col.names=NA, sep=",")
write.table(resTundArea,file="FD_tundra_area-res.csv",
            row.names=TRUE, col.names=NA, sep=",")
write.table(resTundLDMC,file="FD_tundra_ldmc-res.csv",
            row.names=TRUE, col.names=NA, sep=",")
write.table(resTundSLA,file="FD_tundra_sla-res.csv",
            row.names=TRUE, col.names=NA, sep=",")
write.table(resTundHeight,file="FD_tundra_height-res.csv",
            row.names=TRUE, col.names=NA, sep=",")

# Cushion replicates
resCush = dbFD(cushion,abundancesCushion,
               stand.FRic =TRUE,corr = c("lingoes"))
resCushArea = dbFD(cushionArea,abundancesCushion,
               stand.FRic =TRUE,corr = c("lingoes"))
resCushLDMC = dbFD(cushionLDMC,abundancesCushion,
               stand.FRic =TRUE,corr = c("lingoes"))
resCushSLA = dbFD(cushionSLA,abundancesCushion,
               stand.FRic =TRUE,corr = c("lingoes"))
resCushHeight = dbFD(cushionHeight,abundancesCushion,
               stand.FRic =TRUE,corr = c("lingoes"))

##write table with FD metrics
write.table(resCush,file="FD_cushion_all-res.csv",
            row.names=TRUE,col.names=NA, sep=",")
write.table(resCushArea,file="FD_cushion_area-res.csv",
            row.names=TRUE,col.names=NA, sep=",")
write.table(resCushLDMC,file="FD_cushion_ldmc-res.csv",
            row.names=TRUE,col.names=NA, sep=",")
write.table(resCushSLA,file="FD_cushion_sla-res.csv",
            row.names=TRUE,col.names=NA, sep=",")
write.table(resCushHeight,file="FD_cushion_height-res.csv",
            row.names=TRUE,col.names=NA, sep=",")

}


