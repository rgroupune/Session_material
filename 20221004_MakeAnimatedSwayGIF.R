
library(svDialogs)
library(readr)
library(signal)
library(ggplot2)
library(gganimate)
library(janitor)

## Getting started in gganimate: 
# Check out https://gganimate.com/articles/gganimate.html

## Use R's GUI to pick the file you're going to analyse 
# This is useful for a lot of things! 

myFile <- dlg_open(
  title = "Please pick the file you want to analyse",
  filters = dlg_filters[c("txt", "All"), ],
  gui = .GUI
)

# Get the base file name (not including directory) and strip off the file extension 
# Also useful for a lot of things! 
fName <- basename(myFile$res)
Name <- tools::file_path_sans_ext(fName)

# Read in the file (this works for .txt even though it says csv)
swayFile <- read_csv(myFile$res, col_names = TRUE)
swayFile<- clean_names(swayFile) # use janitor to make names sensible (different force plates use different conventions)

# Use "decimate" function from signal toolbox to downsample data to 50 Hz 
# (need to use 20 if data are at 1000 Hz - you'd use 4, for instance, at 200 Hz)
ML <-decimate(swayFile$co_px, 20)
AP <- decimate(swayFile$co_py, 20)

# Convert to mm for nicer plots
# (this assumes data are in m - make it 100 for cm)
AP_mm <- AP*1000
ML_mm <- ML*1000

# Center on first 100 data points 
ML_mm <- ML_mm - mean(ML_mm[1:100])
AP_mm <- AP_mm - mean(AP_mm[1:100])

# Get max and min values for axes 
xmax <- max(ML_mm)
xmin <- min(ML_mm)
ymax <- max(AP_mm)
ymin <- min(AP_mm)

# work out range for each axis 
xRange <- xmax-xmin
yRange <- ymax-ymin

# Get the difference 
rangeDiff <- yRange - xRange 

# Adjust so axes are equal lengths 
if (rangeDiff >0){
  xmax <- xmax+rangeDiff/2
  xmin <- xmin-rangeDiff/2
} else{
  ymax <- ymax+rangeDiff/2
  ymin <- ymin-rangeDiff/2   
}

# Make time vector 
ms <- 1:length(AP_mm)

# Compile into data frame 
df_sway <- data.frame(AP_mm, ML_mm, ms)

# Make plot and animate it 
p1 <- ggplot(df_sway, aes(x = ML_mm, y = AP_mm)) + 
  geom_point(colour = "red", size = .5) + theme_classic()+
  xlim(xmin, xmax)+
  ylim(ymin,ymax)+ 
  coord_fixed(ratio = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=14)) +
  labs(title = paste("Sway animation:", Name, sep = " "), 
       x = "ML sway (mm)", 
       y = "AP sway (mm)") +
  transition_time(ms)+ # Here's the animating bit! 
  shadow_mark(past = T, future=F, alpha=0.3)  # Make points persist

a_gif <- animate(p1, width = 350, height = 350) 
# If you just typed p1 that would work, but this allows you to specify size etc. 
a_gif

# Save the file
saveFname <- paste("SwayAnimation_", Name, ".gif", sep = "")
anim_save(saveFname)

