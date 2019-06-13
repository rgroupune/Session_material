### bring in the data, check to data names, structures, etc.
old.name <- read.table("clipboard", header=TRUE, sep="\t")
old.name
names(old.name) ; str(old.name)


###~~~~~~~~~~~~~~~~~~~~~~~
### set up the data
old.name$RateF <- factor(old.name$Rate) # convert data type
levels(old.name$RateF) 

levels(old.name$OM)
levels(old.name$Fert)

levels(old.name$Soil)
old.name$Soil1 <- factor(old.name$Soil, 
	levels = c("Vertosol", "Chromosol"), # reorder
	labels = c("V", "C")   # rename
)
levels(old.name$Soil1)

levels(old.name$Rep)


###~~~~~~~~~~~~~~~~~~~~~~~
## Plots 
# Root length
xyplot(root.length ~ RateF | OM * Fert * Soil, data = old.name,
	as.table = T, layout=c(4,2,1),
 xlab = "Rate (x recommended rate)",  ylab = "Root length (mm)",
  strip = function(...) strip.default(..., style = 1),
  par.strip.text = list(cex = 0.6, col = 1),
  scales = list(alternating=F, cex=0.8),
  panel = function(x, y) {
	panel.xyplot(x, y, col="grey60")
}	)

# Shoot length
xyplot(shoot.length ~ RateF | OM : Fert * Soil, data = old.name, as.table = T,
 xlab = "Rate (x recommended rate)",  ylab = "Shoot length (mm)",
	layout=c(4,2,1),
  strip = function(...) strip.default(..., style = 1),
  par.strip.text = list(cex = 0.6, col = 1),
  scales = list(alternating=F, cex=0.8),
  panel = function(x, y) {
	panel.xyplot(x, y, col="grey60")
 }	)


#### Linear regression plots
# root length (mm)
xyplot(root.length ~ Rate | OM : Fert * Soil, data = old.name, as.table = T,
 xlab = "Rate (x Recommended rate)",  ylab = "Root length (mm)",
	layout=c(4,2,1), ylim = c(0,120),
  strip = function(...) strip.default(..., style = 1),
  par.strip.text = list(cex = 0.6, col = 1),
  scales = list(alternating=F, cex=0.7, x = list(at=c(0,0.5,1))),
  panel = function(x, y) {
	m1 <- lm(y ~ x)
	x.seq <- data.frame(x=seq(min(as.numeric(x))*0.95,
	 	max(as.numeric(x))*1.05, max(as.numeric(x))/10) )
	pred <- predict(m1, x.seq, se.fit=T)
	grid.polygon(c(x.seq$x,rev(x.seq$x)),
		c(pred$fit + 2*pred$se.fit, rev(pred$fit - 2*pred$se.fit) ),
		gp=gpar(col=0, fill="grey75", alpha = 1), default.units = "native")
	panel.lines(x.seq$x, pred$fit, lty = 1, col="black")
	panel.text(0,30,paste("Pslope=",round(summary(m1)$coef[2,4],5),sep=""),
		cex=0.7, adj=0)
	panel.text(0, 20, paste("b=",round(summary(m1)$coef[2,1],1),
		" \261 ",round(summary(m1)$coef[2,2],2),sep=""),
	 	cex=0.7, adj=0)
  	panel.text(0,10,paste("r.sq=",round(summary(m1)$r.sq, 3)),
	 	cex=0.7, adj=0)
	panel.xyplot(x, y, col="grey60")
	
  	panel.text(0,10,paste("r.sq=",round(summary(m1)$r.sq, 3)),
	 	cex=0.7, adj=0)
 }	)

# shoot length (mm)
xyplot(shoot.length ~ Rate | OM : Fert * Soil, data = old.name, as.table = T,
 xlab = "Rate (x Recommended rate)",  ylab = "Shoot length (mm)",
	layout=c(4,2,1), ylim = c(0,120),
  strip = function(...) strip.default(..., style = 1),
  par.strip.text = list(cex = 0.6, col = 1),
  scales = list(alternating=F, cex=0.7, x = list(at=c(0,0.5,1))),
  panel = function(x, y) {
	m1 <- lm(y ~ x)
	x.seq <- data.frame(x=seq(min(as.numeric(x))*0.95,
	 	max(as.numeric(x))*1.05, max(as.numeric(x))/10) )
	pred <- predict(m1, x.seq, se.fit=T)
	grid.polygon(c(x.seq$x,rev(x.seq$x)),
		c(pred$fit + 2*pred$se.fit, rev(pred$fit - 2*pred$se.fit) ),
		gp=gpar(col=0, fill="grey75", alpha = 1), default.units = "native")
	panel.lines(x.seq$x, pred$fit, lty = 1, col="black")
	panel.text(0,30,paste("Pslope=",round(summary(m1)$coef[2,4],5),sep=""),
		cex=0.7, adj=0)
	panel.text(0, 20, paste("b=",round(summary(m1)$coef[2,1],1),
		" \261 ",round(summary(m1)$coef[2,2],2),sep=""),
	 	cex=0.7, adj=0)
  	panel.text(0,10,paste("r.sq=",round(summary(m1)$r.sq, 3)),
	 	cex=0.7, adj=0)
	panel.xyplot(x, y, col="grey60")
 }	)

 
 
 
 
 
 
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### extra stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANOVA
mod1 <- aov(rl ~ RateF * OM * Fert * Soil, data = old.name)
dplot(mod1) ## look at the diagnostic plots
summary(mod1) ## look at the anova table

mod1 <- aov(sl ~ RateF * OM * Fert * Soil, data = old.name)
dplot(mod1) ## look at the diagnostic plots
summary(mod1) ## look at the anova table

mod1 <- aov(surv ~ RateF * OM * Fert * Soil, data = old.name)
dplot(mod1) ## look at the diagnostic plots
summary(mod1) ## look at the anova table


#### tables of means
with(old.name, tapply(rl, list(RateF, OM, Fert, Soil), mean.na))
with(old.name, tapply(rl, list(RateF, OM, Fert, Soil), se))

with(old.name, tapply(sl, list(RateF, OM, Fert, Soil), mean.na))
with(old.name, tapply(sl, list(RateF, OM, Fert, Soil), se))

with(old.name, tapply(surv, list(RateF, OM, Fert, Soil), mean.na))
with(old.name, tapply(surv, list(RateF, OM, Fert, Soil), se))

#### SHOOT LENGTH
### main term means
with(old.name, tapply(sl, list(RateF), mean.na))
with(old.name, tapply(sl, list(OM), mean.na))
with(old.name, tapply(sl, list(Fert), mean.na))
with(old.name, tapply(sl, list(Soil), mean.na))

### 2-way interacttion means
a <- with(old.name, tapply(sl, list(Fert, RateF), mean.na))
a <- with(old.name, tapply(sl, list(OM, Soil), mean.na))
a <- with(old.name, tapply(sl, list(Soil, Rate), mean.na))
a
(a[2,]-a[1,])/a[2,]*100

### 3-way interacttion means
a <- with(old.name, tapply(sl, list(OM, RateF, Fert), mean.na))
a
(a[2,,]-a[1,,])/a[2,,]*100 # %change OM
(a[,,2]-a[,,1])/a[,,2]*100 # %change Fert

a <- with(old.name, tapply(sl, list(OM, RateF, Soil), mean.na))
a
(a[2,,]-a[1,,])/a[2,,]*100 # %change OM
(a[,,2]-a[,,1])/a[,,2]*100 # %change Soil

a <- with(old.name, tapply(sl, list(OM, Fert, Soil), mean.na))
a
(a[2,,]-a[1,,])/a[2,,]*100 # %change OM
(a[,,2]-a[,,1])/a[,,2]*100 # %change Soil


#### SHOOT LENGTH
with(old.name, tapply(rl, list(RateF), mean.na))
with(old.name, tapply(rl, list(OM), mean.na))
with(old.name, tapply(rl, list(Fert), mean.na))
with(old.name, tapply(rl, list(Soil), mean.na))

a <- with(old.name, tapply(rl, list(OM, Fert), mean.na))
a
(a[2,]-a[1,])/a[2,]*100

a <- with(old.name, tapply(rl, list(Soil, Rate), mean.na))
a
(a[2,]-a[1,])/a[2,]*100


a <- with(old.name, tapply(rl, list(OM, RateF, Fert), mean.na))
a
(a[2,,]-a[1,,])/a[2,,]*100 # %change OM
(a[,,2]-a[,,1])/a[,,2]*100 # %change Fert

a <- with(old.name, tapply(rl, list(OM, Fert, Soil), mean.na))
a
(a[2,,]-a[1,,])/a[2,,]*100 # %change OM
(a[,,2]-a[,,1])/a[,,2]*100 # %change Soil

