######################################
# Publication-quality graphics in R  #
#                                    #
# A demonstration of the basic       #
# elements of creating and annotating#
# graphics in R                      #
# C. E. Timothy Paine                #
# October 2019                       #
######################################

#### Demonstrate the basic elements of creating and annotating a figure. ####
#Set up the plotting region
par(
    mar = c(4, 4, 4, 4), 
    bty = 'n', 
    las = 1, 
    mfrow = c(1, 1))

# Make a plot
plot(
    x = 1:10,
    y = 20:11,
    xlab = "X axis label",
    ylab = "Y axis label",
    col  = rainbow(10),
    pch  = new_vector,
    xlim = c(0, 15),
    cex  = 2)

# Add a custom axis
axis(3, at = 0:5*3, labels = 10^(0:5*3))

# Add shapes, filled or outlined
rect(11, 14, 14, 17, col = NA, border = 'darkgreen', lwd = 2, lty = 2)
polygon(c(1, 5, 10, 10, 5, 1), c(12, 15, 12, 18, 17, 18), col = '#0000FF30', border = NA)


# Add legends
legend("topright", legend = c("Hot", "Cold"), col = c("red", 'blue'), lty = 1, inset = 0.01)
legend(x = 10, y = 18, legend = c("Hot", "Cold"), col = c("red", 'blue'), lty = 1, inset = 0.01)


# add lines
abline(12, 1, col = 'red', lwd = 3)
abline(v = 9, col = 'blue', lwd = 3)
abline(h = 16, col = 'blue', lwd = 3)

# Add line segments
segments(6, 17, 8, 17, lwd = 2)
segments(1:10, 18:9, 1:10, 22:13, col= rainbow(10))

# Add text
text(x = c(2, 4), y = 12, c("misng", "kjshh"), cex = 1.8, pos = 4, srt = 30, family = 'Times', font = 3)
mtext('Figure 1', side = 3, line = 2, adj = 0.05)






#### Applying these tools for a more-real figure #####
# Create some fake data with two predictor variables.
#   pred_con is continuous
#   pred_cat is categorical ('A','B', 'C')
# Response is continuous
dat <- expand.grid(pred_con = rnorm(100, 10, 4),
                   pred_cat = LETTERS[1:3])
dat$resp <- 1.5 * dat$pred_con + 2 * as.numeric(dat$pred_cat) + rnorm(100, 0, 4)

# Show how resp and pred_con are related
plot(resp ~ pred_con, data = dat) # formula syntax

# Improve the axis labels
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)')

# Improve the the X and Y ranges
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(0, 40))

# Try log-transformed y-axis
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(1, 40), log = 'y')



# Use par() to adjust formatting. par() modifies ALL subsequent figures made on the device.
par(las = 1,  # rotate all marginal text to be horizontal
    bty = 'n' # remove the box around the outside
)
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(0, 40))

par(mar = c(4, 6, 1, 1)) # Adjust the amount of space around the outside of the figure. Default is c(5.1, 4.1, 4.1, 2.1)
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = '', xlim = c(0, 25), ylim = c(0, 40))
mtext("Response\n(units)", side = 2, line = 2) # make the y-axis horizontal

# Add in the information from pred_cat as point types
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(0, 40), pch = as.numeric(pred_cat))

# now we need a legend, since we have more than one type of data on the figure!
legend('topleft', pch = 1:nlevels(dat$pred_cat), legend = levels(dat$pred_cat), title =  'Categorical predictor')

# Or, alternatively, add in the information from pred_cat as point colours
plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(0, 40), col = pred_cat)

# Again, we need a legend
legend('topleft', pch = 1, col = 1:nlevels(dat$pred_cat), legend = levels(dat$pred_cat), title = 'Categorical predictor')


# Send a figure to the pdf device
#pdf("~/Desktop/Figure 1 2019 10 11.pdf", paper = 'a4', width = 6, height = 8)
# we need a par statement here, because we're printing to a new device. 
par(las = 1,  # rotate all marginal text to be horizontal
    bty = 'n' # remove the box around the outside
    )

plot(resp ~ pred_con, data = dat, xlab = 'Continuous predictor (units)', ylab = 'Response (units)', xlim = c(0, 25), ylim = c(0, 40), col = pred_cat)
dev.off()
