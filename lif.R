#Introduction to leaflet
library("leaflet")
library(readr)
library(dplyr)
library(RColorBrewer)
library(Hmisc)
m <- leaflet() %>%
        addTiles() %>%
        addMarkers(lng = 174.768, lat=-36.852, popup = "The BD of R") %>%
m

df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet() %>% addCircles()

#using maps package
library(maps)
map()
help(package = "maps")
mapStates = map("state", fill = TRUE, plot = TRUE)
leaflet(data = mapStates) %>% addTiles() %>%
        addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

#playing with randon geospatials
m = leaflet() %>% addTiles()
df = data.frame(
        lat = rnorm(100),
        lng = rnorm(100),
        size = runif(100, 5, 20),
        color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

#japan
air2 <- read_csv("first_vehicle_aug2011.csv")
names(air2)
names(air2) <- c("gridcode","startdate","enddate","pref","city","no_samples",
                 "AvgAirDoseRate","NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                 "SW_nLat","SW_eLong","SE_nLat","SE_eLong")

#Create deciles
air3 <- air2 %>%
        mutate(quantile = ntile(AvgAirDoseRate, 100))
View(air3)
View(quantile(air3$quantile))
table(cut2(air2$AvgAirDoseRate,cuts=c(1,10,20,30,40,50,60,70,80,90,100), g=10))
air4 <- air2 %>%
        mutate(as.factor(quantiles = cut2(air2$AvgAirDoseRate,cuts=c(1,10,20,30,40,50,60,70,80,90,100),levels.mean=TRUE,onlycuts=TRUE, g=10)))
View(air4)
write_csv(air4, path = "air1.csv")
list.files()
a <- leaflet()%>%
        addTiles()%>%
        addMarkers(lng = ~NE_eLong, lat = ~NE_nLat, data = air2)%>%
a

#BASEMAPS
a <- leaflet()%>%
        setView(lng = ~air2$NE_eLong, lat = ~air$NE_nLat, zoom = 12)%>%
        addTiles()
a
#MARKERS
#Cluster the locations where the car collected data
a <- leaflet(data = air2)%>%
        addTiles()%>%
        addMarkers(lng = ~NE_eLong, lat = ~NE_nLat,clusterOptions = markerClusterOptions())
a

# CIRCLES
#Draws blue circles along the vehicle routes
a <- leaflet(data = air2)%>%
        addTiles()%>%
        addCircleMarkers(lng = ~NE_eLong, lat = ~NE_nLat)
a

#POP UPS

#COLOR
# map the Air dose on to the Spatial Points
library(rgdal)
pal <- colorNumeric(
        palette = "Blues",
        domain = air2$AvgAirDoseRate
)

fukulink <- paste(sep = "<br/>",
                 "<br><a href='http://www.tepco.co.jp/en/decommision/index-e.html'>Fukushima Daichi</a></b>",
        "Source of radiations"
)

ji <- leaflet()%>%
        addTiles()%>%
        addPolygons(data = air2,lng = ~SW_eLong, lat = ~SW_nLat,stroke = FALSE, smoothFactor = 0.9,fill= 1,fillColor = "red")%>%
        addPopups(lat = 37.4211, lng = 141.0328,popup = fukulink,
                  options = popupOptions(closeButton = TRUE)) #adds popup
ji

#LINES and Shapes
library(rgdal)
states <- readOGR(dsn = path.expand("cb_2013_us_state_20m.shp"),
                  layer = "cb_2013_us_state_20m", verbose = FALSE) # reads polygon spatialfile

neStates <- subset(states, states$STUSPS %in% c(
        "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(states) %>%
        addTiles(options = tileOptions(maxNativeZoom = 18))%>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                color = ~colorQuantile("Purples", states$AWATER)(AWATER)
)

#leaflet fn,maps data to values to color according to a given palette
color = ~colorQuantile("YlOrRd", states$AWATER)(AWATER) 

#adding Rectangles
ji <- leaflet()%>%
        addTiles()%>%
        addRectangles(data = air4,lng1 = ~SW_eLong, lat1 = ~SW_nLat,lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      fillColor = ~colorFactor("RdYlBu",levels= quantiles))%>%
        addPopups(lat = 37.4211, lng = 141.0328,popup = fukulink,
                  options = popupOptions(closeButton = FALSE)) #adds popup
ji
#Play to see one square/rect
ji <- leaflet()%>%
        setView(lng = 140.8912, lat = 37.54417, zoom = 11) %>%
        addTiles()%>%
        addRectangles(data = air4,lng1 =140.8750,lat1= 37.55667,lng2 = 140.8738, lat2= 37.55750, 
                      color = "blue",stroke = FALSE, smoothFactor = 1)%>%
        addPopups(lat = 37.55667, lng = 140.8750,popup = fukulink,
                  options = popupOptions(closeButton = FALSE)) #adds popup
ji

jio <- c("a","b","c")
leaflet() %>% addTiles() %>%
        addRectangles(
                lng1=-118.456554, lat1=34.078039,
                lng2=-118.436383, lat2=34.062717,
                color = "red",fillColor = ~colorFactor("blues",jio)
)

#Since rect doesn't, try polygons
ji <- leaflet()%>%
        setView(lng = 140.8412, lat = 37.54417, zoom = 11) %>%
        addTiles()%>%
        addPolygons(data = air4,lng = ~NE_eLong, lat = ~NE_nLat,color = "red",
                      fillColor = ~colorFactor(palette="RdYlBu",domain=air4$quantiles))
ji

#RETRY AGAIN, with changed names and using prob
View(air2)
air_quants <- air2 %>%
        mutate(air_qua = cut2(air2$AvgAirDoseRate,cuts=c(10,20,30,40,50,60,70,80,90),levels.mean=TRUE))

hahi <- colorFactor(
        palette = "PuRd",
        domain = air_quants$air_qua
)
quant_plot <- leaflet() %>%
        setView(lat = 37.4211, lng = 141.0328, zoom = 11) %>%
        addTiles()%>%
        addRectangles(data = air4,lng1 = ~SW_eLong, lat1 = ~SW_nLat,lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      color = ~hahi(air_quants$air_qua))%>%
        addPopups(lat = 37.4211, lng = 141.0328,popup = fukulink,
                  options = popupOptions(closeButton = TRUE)) #adds popu
quant_plot
        

