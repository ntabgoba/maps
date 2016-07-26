# Spatial Objects
x <- 1:400
y <- sin(x/10)* exp(x * -0.01)
plot(x,y)
z <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap")
lapply(z, library, character.only = TRUE)
# library(rgdal)
library(rgdal)
lnd <- readOGR(dsn = "data", layer = "london_sport")
head(lnd@data, n=2)
mean(lnd$Partic_Per)
plot(lnd)
plot(lnd@data)
lnd@data[lnd$Part_Per < 15, ]
lnd[1:2, 1:3]

#select zones where sports participation is between 20 and 25%
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel, ])
head(sel)
plot(lnd, col = "lightgrey") # plot the london_sport object
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ], col = "turquoise", add = TRUE)
#different slots, the key slots being @data and 
# @polygons (or @lines for line data).
# The data slot can be thought of as an attribute
# table and the geometry slot is the polygons that 
# make up the physcial boundaries. Specific slots are accessed 
# using the @ symbol
plot(lnd, col = "grey")
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 

# method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,] # the selection is too big!
# test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer

##
##
##
# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Add text to the plot!
text(coordinates(cent_lnd), "Central\nLondon")

#Create an outline of the London area by merging all the polyogns
# in the lnd object
london = gUnaryUnion(lnd, lnd$dummy)
london = SpatialPolygonsDataFrame(london, data.frame(dummy = c("london")), match.ID = FALSE)
plot(london)

# Find the centre of the london area
centrelondon = gCentroid(london, byid = TRUE)

# create coordinates to store the start and end points of the lines
c1 = c(centrelondon$x, centrelondon$x)
c2 = c(90, -90)
c3 = c(90, -90)
c4 = c(centrelondon$y, centrelondon$y)

# simple line strings using the created coordinates
L1 = Line(cbind(c1, c2))
L2 = Line(cbind(c3, c4))

#create the lines 
Ls1 <- Lines(list(L1), ID = "a")
Ls2 <- Lines(list(L2), ID = "b")
# convert the lines into SpatialLines
Ls1 <- SpatialLines(LinesList = list(Ls1)) 
Ls2 <- SpatialLines(LinesList = list(Ls2))
#convert the lines into SpatialLines
# convert again into SpatialLinesDataFrame
Longitude = SpatialLinesDataFrame(Ls1, data.frame(Z = c("1", "2"), row.names = c("a","b"))) 
Latitude = SpatialLinesDataFrame(Ls2, data.frame(Z = c("1", "2"), row.names = c("a","b")))
# arguments to test whether or not a coordinate is east or north of the centre
east <- coordinates(lnd)[,1] > Longitude@lines[[1]]@Lines[[1]]@coords[,1][1] 
north <- coordinates(lnd)[,2] > Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]
# test if the coordinate is east and north of the centre
lnd@data$quadrant[east & north] <- "northeast"

names(lnd)
str(lnd$Partic_Per)
summary(lnd)


## Part III
# Creating and manipulating spatial data
vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))
class(vec)
class(df)
# same logic works with spatial data
#To create a SpatialPoints
# object, for example, the input coordinates must be supplied in a matrix:
mat <- as.matrix(df)
sp1 <- SpatialPoints(coords = mat)
#fundamental data types for spatial data. 
#(The others are lines, polygons and pixels,
# which can be created by SpatialLines, SpatialPolygons and SpatialPixels, respectively
class(sp1)
spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)

## Coordinate Reference System (CRS)- defines where spatial
# objects are place on the Earth Surface.

# Part IV: Making maps with tmap, ggplot2 and leaflet
vignette(package = "tmap")
vignette("tmap-nutshell")
library(tmap,ggplot2)
#tmap

qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues")

qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = c("Blues"),ncol = 2)
tm_shape(lnd) +
        tm_fill("Pop_2001", thres.poly = 0) +
        tm_facets("name", free.coords=TRUE, drop.shapes=TRUE) +
        tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)


