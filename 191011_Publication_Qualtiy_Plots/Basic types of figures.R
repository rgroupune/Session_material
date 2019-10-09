######################################
# Publication-quality graphics in R  #
#                                    #
# A demonstration of the basic       #
# types of graphs available in R     #
# C. E. Timothy Paine                #
# October 2019                       #
######################################

###########################################
# These are the Basic commands for plotting
# plot()                       # create a new plot of any type
# hist()                       # create a histogram
# lines()                      # add straight lines to an existing plot
# points()                     # add points to an existing plot.
# abline()                     # add a line of the form ax + b to an existing plot.
# segments(), arrows(), rect() # Add (a sequence of) line segments, arrows, or rectangles to an existing plot.
# interaction.plot()           # Easily create interaction plots
###########################################


# Pre-load some libraries...
library(lattice)
library(lme4)

#Load data, and clean up dates
# These data come from Paine et al. 2009. "Supplemental irrigation increases seedling performance and diversity in a tropical forest". Journal of Tropical Ecology
dat <- read.csv("data for plotting.csv")
dat$date <- as.factor(dat$date)
dat.4 <- dat[dat$date == 4,]  # pull a subset of the dataset -- only include rows from the 4th time period.

# Demonstrate a scatter plot
plot(dat.4$density, dat.4$richness, ylab = "Species Richness", xlab = "Seedling Density") 
plot(richness ~ density, data = dat.4, ylab = "Species Richness", xlab = "Seedling Density") # alternative syntax, often better
plot(jitter(dat.4$density), dat.4$richness, ylab = "Species Richness", xlab = "Seedling Density")		#spread out points a bit - makes obvious that most plots have no seedlings

# Demonstrate how to colour points on a figure
plot(richness ~ jitter(density), data = dat.4, ylab = "Species Richness", xlab = "Seedling Density", col = ifelse(watered == 'wet', 'blue', 'red'))
# any time you have more than one type of data on a figure, you need a legend!
legend('topright', col = c('red', 'blue'), legend = levels(dat$watered), pch = 1, title = "Treatment applied")

# Demonstrate a boxplot
plot(density ~ watered, data = dat.4, ylab = "Seedling Density", xlab = "Watering Treatment") #makes a boxplot because predictor variable is of type Factor
# Note the formula specification of what to plot. 
# This is the same way you write formulas for statistical functions.
boxplot(density ~ watered + date, data = dat, ylab = "Seedling Density", xlab = "Watering Treatment") 
# note that it's easy to change colors, line types, etc. 
boxplot(density ~ watered + date, data = dat, ylab = "Seedling Density", xlab = "Watering Treatment", col = c("pink", "light blue")) 
# note that the arrangement of boxes affects interpretation
boxplot(density ~ date + watered, data = dat, ylab = "Seedling Density", xlab = "Watering Treatment") 
# note that it's easy to change colors, line types, etc. 
boxplot(density ~ date + watered, data = dat, ylab = "Seedling Density", xlab = "Watering Treatment", col = rep(c("pink", "light blue"), each = 3)) 

# Demonstrate a Histogram
hist(dat.4$richness, main = "Species richness per m^2")              # make a histogram
hist(dat.4$richness, main = "Species richness per m^2", breaks = 10) # make ~ 10 bars
abline(v = mean(dat.4$richness), col = "red")                        # add a line for the mean. "v=" indicates that you want a Vertical line.
abline(v = quantile(dat.4$richness, c(0.25, 0.50, 0.75)), col = "blue", lty = c(2, 1, 2), lwd = 2) # add blue lines for the median and quartiles. You can add more than 1 line with a single command

# Demonstrate Interaction Plots
with(dat, interaction.plot(date, watered, richness, ylim = c(3, 6), leg.bty = "o")) 
#interaction.plot() has no way to specify a "data =" arguement,so it's convenient to use the with() syntax. Note that here, we specify the desired limits on the y-axis

# Demonstrate diagnostic plots... first run an ANOVA and an ANCOVA to have something to plot...
lm.density  <-lm(log(density + 1) ~ watered * date, data = dat)	
lme.density  <-lmer(log(density + 1) ~ watered * date + (1 | plot/subplot),	data = dat)
# note that there's no output from these commands - the model fit is saved to the variables "lm.density" and "lme.density". 

#View sumarized model output with
summary(lm.density)
#and anova tables with
anova(lm.density)

#For lm() linear models, obtaining diagnostic plots is very simple
par(mfrow = c(2, 2)) # set the plotting area to four panels (2 rows by 2 columns)
plot(lm.density)

#For lmer() models, it's a bit more complex, since plotting a lme object returns only a residual plot, ie
plot(lme.density)
# here's a few more things you can plot
par(mfrow = c(1, 2)) # set the plotting area to four panels (2 rows by 2 
plot(fitted(lme.density), resid(lme.density), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")#residual plot
qqnorm(resid(lme.density)); qqline(resid(lme.density), col = "red") ##QQ plot

				
####Lattice graphs can be really powerful for big and multivariate datasets. ####
# plot relationship for each plot seperately (4 watered plots, 4 unwatered)
xyplot(richness ~ density | plot, groups = watered, data = dat, 
       strip = strip.custom(strip.levels = c(T, T)))

#### Elaborate Lattice Example ####
# this is not fully commented - we can go through the commands and options if you want.
# prepare and summarize the data
long <- reshape(dat, varying = list(6:11), direction = "long", drop = c("calobr", "carplo", "casearia", "geniam", "hymein", "iriade", "otobpa", "socrex", "sponmo", "vitecy", "wettau", "year"), times = names(dat[6:11]))
water <- long$watered; 
date <- as.numeric(as.character(long$date)); 
response <-long$time
sum <- summarize(long$recruits, by = llist(response, date, water), smean.cl.boot)
sum$month <- 0
sum$month[sum$date == 2] <- 3
sum$month[sum$date == 3] <- 10
sum$month[sum$date == 4] <- 22
names(sum)[4] <- "Mean"

setTrellis()
xYplot(
	Cbind(response = Mean, Lower, Upper) ~ month | response, 
	groups = water, 
	data = sum, 
	type = "l", 
	ylab = "",
	xlim = c(-2, 24),
	label.curves = F,
	col = c("red", "blue"), 
	scales = list(alternating = F, tck = c(1, 0),
		y = list(rot = 0),
		x = list(tick.number = 4, at = c(0, 3, 10, 22), labels = c("Mar 05", "Jul 05", "Jan 06", "Jan 07"), rot = 45)),
	)    
mtext("Sown Seedlings", outer = T, line = -1)     



	
# set up a publication-quality figure
par(
	mfrow = c(3, 5), 
	pty= "m", 
	mar = c(3, 3, 1, 1), 
	oma = c(1, 1, 1, 1),
	las = 1, 
	xpd = NA
	)    
for(i in c(1:15)){
	X.i <- runif(100)# replace with real data....
	Y.i <- rnorm(100)
	line.lm <- lm(Y.i~X.i)
	plot(X.i, Y.i, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(-3, 3), axes = F, cex = 0.8, col = "gray", pch = 19)
	R2  <- bquote(paste(r^2==  .(sprintf("%1.2f", summary(line.lm)$r.squared))))
	eqn <- bquote(paste(Y== .(sprintf("%1.2f", coef(line.lm)[1])), "X", .(sprintf("%+1.2f", coef(line.lm)[2])), ))
	Xes <- data.frame(X.i = seq(min(X.i), max(X.i), length = 100))
	predicted <- predict(line.lm, newdat = Xes)
	mtext(R2,  cex = 0.7, line = 0, adj = 0.95)
	mtext(eqn, cex = 0.7, line = -1, adj = 0.95)
	mtext(paste0(LETTERS[i], ")"), adj = 0.05, line = 0, font = 2)
		if(i %% 3 == 1){
			axis(2)
			} else {
			axis(2, labels = F)
			}
		if(i > 10){
			axis(1)
			} else {
			axis(1, labels = F)
			}
		lines(Xes$X.i, predicted)
	}    
title(ylab = "Y AXIS LABEL", xlab = "X AXIS LABEL ", outer = T, line = -1) 
mtext(text = "Figure 2", adj = 0, outer = T)


	
	
