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

