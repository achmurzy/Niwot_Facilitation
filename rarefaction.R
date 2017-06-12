library(gdata)

data <- read.xls("stems.xlsx", header = TRUE)
plotCodes <- data[,1]
extractPlot <- function(x) 	#Get array of plot codes to color data
	{ lengthC = nchar(toString(x)) 
	char = substr(x, lengthC, lengthC) 
	if (char == 'N' || char == 'S') 'T' else 'C' }
plotCodes <- sapply(plotCodes, extractPlot)
plotCodes <- factor(plotCodes)

individuals <- data[,37]
data <- data[,-c(1, 37)]
richness <- apply(data, 1, function(x) sum(!is.na(x)))
df <- data.frame(Individuals=individuals, Richness=richness, Plot=plotCodes)

p_range <- data.frame(Individuals=seq(10, max(individuals), 10))

#Roughly, C is the upper limit (total species count) and k is the rarefaction slope
#Community-wide
fit <- nls(Richness ~ C*(1-exp(k*Individuals)), data=df, 
	algorithm="port", start=c(C=30,k=-0.1),
	lower=c(C=0,k=-Inf), upper=c(C=Inf,k=0))
summary(fit)
prediction <- predict(fit, p_range)

#Cushion communities
df_c <- subset(df, df$Plot == 'C')
fit_c <- nls(Richness ~ C*(1-exp(k*Individuals)), 
	data=df_c, 
	algorithm="port", start=c(C=30,k=-0.1),
	lower=c(C=0,k=-Inf), upper=c(C=Inf,k=0))
summary(fit_c)
prediction_c <- predict(fit_c, p_range)

#Tundra communities
df_t <- subset(df, df$Plot == 'T')
fit_t <- nls(Richness ~ C*(1-exp(k*Individuals)), 
	data=df_t, 
	algorithm="port", start=c(C=30,k=-0.1),
	lower=c(C=0,k=-Inf), upper=c(C=Inf,k=0))
summary(fit_t)
prediction_t <- predict(fit_t, p_range)

png("Rarefaction_fit.png")
plot(individuals, richness, col=plotCodes)
lines(p_range$Individuals, prediction, col="blue", lty=2)
lines(p_range$Individuals, prediction_c, col="black", lty = 2)
lines(p_range$Individuals, prediction_t, col="red", lty = 2)
legend('topright', legend=levels(plotCodes), col=1:length(plotCodes), pch=1)
dev.off()
#text(0, 0, sprintf("C: Pr(>|t|) < 0.001\nk: Pr(>|t|) < 0.001"))

#The rarefaction curve shows what we expect:
#Fewer individuals in cushions result in lower richness for the same area
#Why are there fewer individuals in cushions?
#Support for stress-tolerant facilitation

