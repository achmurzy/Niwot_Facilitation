#Creates graphics for output from package 'FD'
#Compare indicator species trait values to community values

library(ggplot2)
library(gridExtra)
library(reshape)
library(plyr)

#Expects only the filename of results to analyze
filename <- commandArgs(trailingOnly=TRUE)
filename <- "FD_tundra_all-res"
print(filename)

data <- read.csv(paste(filename, ".csv", sep=""))
treatFac <- rep("T", nrow(data))
data$Treat <- treatFac

data_ind <- read.csv(paste(filename, "_ind.csv", sep=""))
indFac <- rep("I", nrow(data_ind))
data_ind$Treat <- indFac

traits <- c('CWM.SLA', 'CWM.LDMC', 'CWM.HEIGHT_cm', 'CWM.AREA_cm2')

df <- rbind(data, data_ind)
newdat <- melt(df, id.vars="Treat", measure.vars=traits)

testFunc <- function(x) 
	{ t <- t.test(value~Treat, data=x)
	with(t, data.frame(statistic, p.value)) }
resultado <- ddply(newdat, "variable", testFunc)

boxplots <- ggplot(data=newdat, aes(x=Treat, y=value))
boxplots <- boxplots + geom_boxplot(aes(fill=Treat)) +
		facet_wrap(~variable,scales="free")
boxplots <- boxplots + geom_label(aes(x=-Inf, y=-Inf, vjust=-1,hjust=-1, label=paste("P<=", format(p.value, digits=4))),data=resultado,inherit.aes=FALSE)
print(boxplots)
ggsave(paste(filename,".png",sep=""), plot=boxplots,device='png')
#png(paste(filename,".png",sep=""))
#grid.arrange(p, q, r, s, nrow=2, ncol=2)
#dev.off()

#message("Press Return To Continue")
#invisible(readLines("stdin", n=1))
if(FALSE)
{p <- ggplot(data=df, aes(x=Treat, y=CWM.SLA))
p <- p + geom_boxplot(aes(fill=Treat), position="dodge")

q <- ggplot(data=df, aes(x=Treat, y=CWM.LDMC))
q <- q + geom_boxplot(aes(fill=Treat), position="dodge")

r <- ggplot(data=df, aes(x=Treat, y=CWM.HEIGHT_cm))
r <- r + geom_boxplot(aes(fill=Treat), position="dodge")

s <- ggplot(data=df, aes(x=Treat, y=CWM.AREA_cm2))
s <- s + geom_boxplot(aes(fill=Treat), position="dodge")}

