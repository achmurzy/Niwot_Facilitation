library(gdata)
library(ggplot2)
library(ggpmisc)

tData <- read.csv("FD_tundra_all-res.csv")
cData <- read.csv("FD_cushion_all-res.csv")
fData <- rbind(tData, cData)
fData <- data.frame(Plot=fData$X, FDis=fData$FDis)

stems <- read.xls("stems.xlsx")
areas <- read.csv("CushionAreas.csv")

#This is why you take care of your code. You already wrote this code
#We don't like writing code twice. 
#Use plot codes to index area to get stem densities
#Regress on both community types to get slopes.

extractPlot <- function(x) 	#Get array of plot codes to color data
	{ char = returnPlot(x) 
	if (char == 'N' || char == 'S') 'T' else 'C' }
returnPlot <- function(x)
	{ lengthC = nchar(toString(x)) 
	char = substr(x, lengthC, lengthC) }
codes <- sapply(stems$CODE, extractPlot)

extractNumber <- function(x) 	#Get array of plot numbers
	{ lengthC = nchar(toString(x)) 
	char = substr(x, 0, lengthC-1) 
	as.numeric(char) }
plots <- sapply(stems$CODE, extractNumber)

extractSite <- function(x) #Get site number
	{ code <- returnPlot(x)
	count <- if(code == 'N') 2 else {if (code == 'S') 3 else 1}
	base <- extractNumber(x)
	((base-1)*3) + count
	}
sites <- sapply(stems$CODE, extractSite)

data <- data.frame(Site=sites, Plot=plots, Code=codes)

divideByArea <- function(x) #Pass stems, get PLOT, divide by area 
	{ 
	plot <- data$Plot[x]
	stems$Total[x] / areas$Area[plot]
	}
#How it should be done. We know exactly how many rows, so execute along
data$Density <- sapply(seq(1,90), divideByArea)

sort <- order(data$Site)
data <- data[sort,]

sortDis <- order(fData$Plot)
fData <- fData[sortDis,]
data$FDis <- fData$FDis

tundra <- which(data$Code == 'T')
cushion <- which(data$Code == 'C')

#ggplot formulae don't understand data columns...
form <- y~x
print(form)

lines <- ggplot(data=data, aes(x=Density, y=FDis, color=Code)) + 
	geom_point(alpha=0.5) +
	geom_smooth(method = 'lm', formula=form, se=FALSE) + 
	stat_poly_eq(aes(label = paste(..rr.label..)), 
       		label.x.npc = "right", label.y.npc = 0.15,
       		formula=form, parse = TRUE, size = 3)+
	stat_fit_glance(method = 'lm',
		method.args = list(formula = form),
                geom = 'text',
		aes(label = paste("P-value = ", signif(..p.value.., 					digits = 4), sep = "")),
       label.x.npc = 'right', label.y.npc = 0.35, size = 3)
print(lines)
ggsave("FDis_density.png", plot=lines,device='png')

#	geom_smooth(data=data[cushion,], 
#		method = 'lm',
#		aes(color=Code), 
#		se=FALSE) +
#tm <- lm(form, data=data, tundra)
#cm <- lm(form, data=data, cushion)
	

