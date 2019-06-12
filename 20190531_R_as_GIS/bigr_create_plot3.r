### This is the R script to create two shape files, combine them, add CDAX data points, create
### the grid, krig the data and then plot the map.

### I have not included the change over time code. The data is available in the csv file
### CDAX_92_Change_1.csv
### You use the day0 code but now have the kriging script use Height4 for day 4, Height8 for day 8
### and Height12 for day 12.

### Modify the spplot code to reflect the new kriging. Note "var1.pred" is always the same in this code.

## Step 1. create shapefile for plot3

setwd("N:/31 May talk/")  ## this should be the location of your R script (I use N:/ because this is my external HDD)
locate_file<-c("N:/31 May talk/Shapefiles/")  ## this should be the location of your shapefiles

## librarys used
library(rgdal)
library(sp)
library(maptools)

## create a colour palette for maps
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299) ## set up colour ramp for map

## load large shapefile for all plots (this is the dGPS data)
bigr<-readOGR(dsn=locate_file,layer="BigRidge_POIs_2019-03-19")
# the coordinates are in Easting and Northing
bigr1<-data.frame(bigr) # make data.frame

## plot 3 (upper and lower plots)
UR3_1<-c(bigr1[25,2],bigr1[25,3]) # coordinates for upper ryegrass plot 3
UR3_2<-c(bigr1[26,2],bigr1[26,3])
UR3_3<-c(bigr1[41,2],bigr1[41,3])
UR3_4<-c(bigr1[42,2],bigr1[42,3])
## create polygon and save to file
bigr1_UR3<-rbind(UR3_1,UR3_2,UR3_3,UR3_4)
xUR3 <- bigr1_UR3[,1]
yUR3 <- bigr1_UR3[,2]
xyUR3 <- cbind(xUR3, yUR3)
pUR3 = Polygon(xyUR3)
psUR3 = Polygons(list(pUR3),1)
spsUR3 = SpatialPolygons(list(psUR3))
plot(spsUR3)
spsUR3
proj4string(spsUR3) = CRS("+init=epsg:32756")
dUR3 = data.frame(f=1)
spdfUR3 = SpatialPolygonsDataFrame(spsUR3,dUR3)
spdfUR3
summary(spdfUR3)
writeOGR(spdfUR3, dsn=locate_file,layer="UR3_BigR", driver="ESRI Shapefile")

LR3_1<-c(bigr1[41,2],bigr1[41,3]) # coordinates for lower ryegrass plot 3
LR3_2<-c(bigr1[42,2],bigr1[42,3])
LR3_3<-c(bigr1[47,2],bigr1[47,3])
LR3_4<-c(bigr1[48,2],bigr1[48,3])
## create polygon and save to file
bigr1_LR3<-rbind(LR3_1,LR3_2,LR3_3,LR3_4)
xLR3 <- bigr1_LR3[,1]
yLR3 <- bigr1_LR3[,2]
xyLR3 <- cbind(xLR3, yLR3)
pLR3 = Polygon(xyLR3)
psLR3 = Polygons(list(pLR3),1)
spsLR3 = SpatialPolygons(list(psLR3))
plot(spsLR3)
spsLR3
proj4string(spsLR3) = CRS("+init=epsg:32756")
dLR3 = data.frame(f=1)
spdfLR3 = SpatialPolygonsDataFrame(spsLR3,dLR3)
spdfLR3
summary(spdfLR3)
writeOGR(spdfLR3, dsn=locate_file,layer="LR3_BigR", driver="ESRI Shapefile")

## now combine upper and lower plot 3 polygons

Plot3 <- sp::SpatialPolygons(list(sp::Polygons(list(pUR3), ID = "UR3"),
                                     sp::Polygons(list(pLR3), ID = "LR3")))

### note "sp:: Polygons" means use the Polygons command from library sp

plot(Plot3)
axis(1)
axis(2)

## now save as shapefile

shapefile(Plot3, "filename.shp") ## this command comes from maptools. make sure filename also has drive and folder location

### add CDAX points (GPS data from CDAX)

### Note CDAX points are in lat/lon. Must convert to Easting, Northing

### load in CDAX data file (already edited for this plot)

cdax92<-read.csv('CDAX_92_Change_1.csv',sep=',',header=TRUE)
attach(cdax92)
names(cdax92)
head(cdax92)
nrow(cdax92)

## since this script already has created Plot3.shp there is no need to load it.

# Setting existing coordinate as lat-long system
cord.dec3 = SpatialPoints(cbind(cdax92$LONG, cdax92$LAT), proj4string = CRS("+proj=longlat"))
# Transforming coordinate to UTM using EPSG=32756 for WGS=84, UTM Zone=56M, Southern Hemisphere
cord.UTM3 <- spTransform(cord.dec3, CRS("+init=epsg:32756"))
cord.UTM3
points(cord.UTM3,pch=20) # draws ponits on plot (if plot already active)

### now to prepare for kriging
## make points a data frame
xy3<-data.frame(cord.UTM3) ## create a data frame
colnames(xy3)<-c('X','Y') 33 name the columns X and Y
cdax3_92b<-cbind(cdax92,xy3) ## combine the coordinates with data

### have to do this for kriging
p3_1<-cdax3_92b
coordinates(p3_1)<-~X+Y
## make points same coordinate projection as shapefile
proj4string(cord.UTM3) <- proj4string(Plot3)

### create the grid

bb <- bbox(Plot3) # get the shapefile corners
cs <- c(.5, .5) # set cell size for grid, the smaller the higher the resolution

cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd

sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(Plot3)))
summary(sp_grd)
## overlay points on grid
over(cord.UTM3, sp_grd)
#To plot the shapefile and the grid with the cell IDs:

plot(Plot3)
spgrd3 <- SpatialPoints(sp_grd, proj4string = CRS(proj4string(Plot3)))
spgrdWithin3 <- SpatialPixels(spgrd3[Plot3,])
plot(spgrdWithin3, add = T)
spgrdWithin3<-as(spgrdWithin3,"SpatialPixels")

### now do kriging
v.ok3 = variogram(HEIGHT~1, p3_1)  ## no trend
ok.model3 = fit.variogram(v.ok3, vgm("Exp"))
##plot the variogram fit
plot(v.ok3,ok.model3)
proj4string(p3_1)<-CRS(proj4string(Plot3))
h.ok3 = krige(HEIGHT~1, p3_1, spgrdWithin3, model = ok.model3) ## no trend

## plot the map to device (screen)

pts3 = list("sp.points", p3_1, pch = 20, col = "black") ## this is the CDAX data points
spplot(h.ok3, "var1.pred",sp.layout = list(pts3),col.regions=my_palette,main = "Pasture height (mm) - R3 (day 0)",par.settings = list(axis.line = list(col = 'transparent')))

## save map to object

day0<-spplot(h.ok3, "var1.pred",sp.layout = list(pts3),col.regions=my_palette,main = "Pasture height (mm) - R3 (day 0)",par.settings = list(axis.line = list(col = 'transparent')))


### repeat for day4 (=cdax92$Height4), day8 (=cdax92$Height8) and day12 (=cdax92$Height12)

### calculations for change in height over time

#cdax92$Change04<-cdax92$Height4-cdax92$HEIGHT
#cdax92$Change08<-cdax92$Height8-cdax92$HEIGHT
#cdax92$Change012<-cdax92$Height12-cdax92$HEIGHT
#cdax92$Change48<-cdax92$Height8-cdax92$Height4
#cdax92$Change812<-cdax92$Height12-cdax92$Height8

### repeat code to create maps for change in height

