# Making maps

# Helpful references
# Leaflet instructions:  https://rstudio.github.io/leaflet/  
# Leaflet package details:  https://cran.r-project.org/web/packages/leaflet/leaflet.pdf
# API: https://leafletjs.com/reference-1.4.0.html
# Leaflet tutorials: https://leafletjs.com/examples.html

#you may also need additional packages - google it!

# Required packages 
install.packages("leaflet")
library(leaflet)
install.packages("dplyr")
library(dplyr)


# Example 1:  Location map

# setView (sets centre location - you coudl get this from google earth etc.), can adjust zoom level)
leaflet() %>% addTiles() %>% setView(lng = 151.639405, lat = -30.487969, zoom = 10)

# Add markers and a popup (which can be used if you save as a web page)
leaflet() %>% addTiles() %>% setView(lng = 151.639405, lat = -30.487969, zoom = 10) %>% addMarkers(lng = 151.639405, lat = -30.487969, popup = "UNE")

# Change background using ProviderTiles
# Options: http://leaflet-extras.github.io/leaflet-providers/preview/
leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% setView(lng = 151.639405, lat = -30.487969, zoom = 10) %>% addMarkers(lng = 151.639405, lat = -30.487969, popup = "UNE")

##################
### Activity 1 ###
##################

# A
# Create a map using a particular location (e.g, your hometown) as the centre
# Adjust the zoom level to an appropriate level
# No popups 
# Select at least two different backgrounds for your map

########################################################

# Example 2:  Earthquake locations in the Pacific Region

# View dataset (inbuilt in R)
quakes

# View column labels in quakes dataset
names(quakes)

# Create a basic map using the first 20 records
leaflet(data = quakes[1:20,]) %>% addTiles() %>% addMarkers(~long, ~lat)

# Same map with popups = magnitude (hover over them to view)
leaflet(data = quakes[1:20,]) %>% addTiles() %>% addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

# Same map with markers as circles - radius is equal to 0.25 of the magnitude, no popups
leaflet(data = quakes[1:20,]) %>% addTiles() %>% addCircleMarkers(~long, ~lat, radius = 0.75*quakes$mag)

# Cluster your observations
leaflet(quakes) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())

# Change icons and colours
# first 20 quakes
df.20 <- quakes[1:20,]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>% addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))

# Use awesome icons ... however I haven't explored this yet!
# If it's of interest to you (i.e., you might use maps for blogs and or need funky icons this is 
# the place to start)
# ion https://ionicons.com/
# fontawesome  https://fontawesome.com/icons?from=io

#################
## Activity 2 ###
#################

# A
# Use the quakes dataset to create an "interesting" map - think of changing colour scheme,
# icons (if you want), background, and using more than just 20 observations (or different observations than I've picked)

# B
# Use spinner dolphin dataset to create an "interesting" map
# Loading external datasets reminder:
# Spinner <- read.csv(file.choose())
# You'll need to adjust the names of the lat and long columns to match the data files
# Ideas: Change the background and adjust markers to group size or colour by year

# C
# If you've got your own dataset that has lat and long coordinates create your own map.
# Notes:  Your input dataset needs to have columns called Long and Lat with 
# locations given in decimal degrees for your sites.  You can also 
# include attributes of sites such as environmental measurements, 
# species numbers (population size) etc and use these in your figure.

###################################################################

# Heatmaps

#################
## Activity 3 ###
#################

# Online tutorial to follow - uses inbuilt dataset mtcars
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# Be sure to load the following packages to complete:
install.packages("reshape2")
library("reshape2")
install.packages("ggplot2")
library("ggplot2")

# A
# Use the IUCN_PICT dataset to create heatmaps ... by IUCN category and also by Pacific
# Island or Country
# Hint:  (i) You'll need to select only numerical variables in each case, (ii) You'll need
# to transpose the dataset for the second option

 
