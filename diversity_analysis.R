print("Starting diversity analysis")
#species diversity metrics
div = diversity(comp, index = "shannon",)
SR = specnumber(comp)

mod1 = aov(div ~ dat$Treat2)
summary(mod1)

mod2 = aov(SR ~ dat$Treat2)
summary(mod2)


par(mfrow=c(1,2))
boxplot(SR~dat$Treat2, xlab ="Tundra location", ylab="Species richness", col = c("lawngreen", "yellow4"))
text(0.68,20,"P<0.001")

plot(div~dat$Treat2, xlab ="Tundra location", ylab="Shannon's diversity", col = c("lawngreen", "yellow4"))
text(0.68,2.65,"P<0.001")

