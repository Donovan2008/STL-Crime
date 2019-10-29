# install the library packages
library(rgdal)
library(sp)
library(sf)
library(raster)
library(leaflet)
library(leafpop)
library(mapview)
library(tidyverse)
library(censusxy)
library(tidycensus)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(fasttime)
library(sparklyr)
library(lubridate)
library(maps)

############################## Temporary
#setwd("C:/users/jd1/Desktop/ALL R Stuff and Research/Test_data_table")

#####################################################
# 1.  Collect St Louis City crime UCR statistics
# pull in state coordinate system files from st louis police reports using data.table
stl.city.Group2018 <- fread("C:/users/jd1/Desktop/ALL R Stuff and Research/Test_data_table/Group2018.csv", stringsAsFactors=FALSE)

######### 2. look at the structure of the data
str(stl.city.Group2018)

### 3. Get rid of unnecessary variables - flag and count variables are not needed
stl.city.Group2018 <- stl.city.Group2018[, c(1:3, 9:14, 17:20)]

## 4. Look at the characteristics of the data
summary(stl.city.Group2018)    
#### Note 1: XCoord and YCoord coordinates are based on State Plane North American Datum 1983 (NAD83) format that will have to be adjusted to lat/long data
#### Note 2: Some of the  XCoord and YCoords have values of O.  This will need to be accounted for later

## 5. Split the DateOccur variable and create Date and Time variables to add to the data set
## Make the data set a data frame
######## best way to split date/time and still keep as characters
stl.city.Group2018$Date <- format(as.POSIXct(stl.city.Group2018$DateOccur,format="%m/%d/%Y %H:%M"),"%m/%d/%Y")
stl.city.Group2018$Time <- format(as.POSIXct(stl.city.Group2018$DateOccur,format="%m/%d/%Y %H:%M"),"%H:%M")
## Check the class of the object and make sure it's a dataframe
class(stl.city.Group2018)
###############################################
## 6. Take a peek at the data set values and number of records (87,870)
glimpse(stl.city.Group2018)


## 7. Account for the inconsistent data  Clean up those X/Y zeros
## Separate the records that have "zeros" values in XCoord, YCoord (2327 records) for a different type of processing.  
stl.city.Group2018.zeros <- stl.city.Group2018[c(XCoord < 1)]
view(stl.city.Group2018.zeros) # there are 2327 that cannot be processed


## 8.  Select another set of records (85,543 records) that can be converted to lat/long directly
stl.city.Group2018.complete <- stl.city.Group2018[c(XCoord > 1)]
view(stl.city.Group2018.complete)
# 
#############################################################




#########
################################################
# 9. Filter out 0nly carjackings (UCR = 38111).  Begins specific crime analysis
carjack.complete <- filter(stl.city.Group2018.complete, Crime == 38111 )
carjack.complete



## 10. Now aAdd lat/long to the "complete" dataset by converting NAD83 to lat/long
##  this converts all the State Plane Coordinate values into lat and long 
nad83_coords <- data.frame(x=carjack.complete$XCoord, y=carjack.complete$YCoord) # My coordinates in NAD83
nad83_coords <- nad83_coords *.3048  ## Feet to meters
coordinates(nad83_coords) <- c('x', 'y')
proj4string(nad83_coords)=CRS("+init=epsg:2815")
coordinates_deg <- spTransform(nad83_coords,CRS("+init=epsg:4326"))
coordinates_deg
str(coordinates_deg)
class(coordinates_deg)
# add converted lat-lonf and convert to numeric values
carjack.complete$lon <- as.numeric(coordinates_deg$x)
carjack.complete$lat <- as.numeric(coordinates_deg$y)
class(carjack.complete)
glimpse(carjack.complete)
####################


## 11.  Now convert the incomplete data that had no coordinates (0) using censusxy library   This creates an sf file and plots out points
############################################################################################
# Filter out carjackings (UCR =38111) for 0 Coords  in put is 20 instances
carjack.zeros <- filter(stl.city.Group2018.zeros, Crime == 38111 )

#### Can only convert 13 instances with censusxy
data <- mutate(carjack.zeros, address.comb = paste(CADAddress, CADStreet, sep = " "), city = "St Louis", state = "MO")
## Goes to the US Census Bureau to look up the crime address reported by police record and returns lat/long
carjack_sf <- cxy_geocode(data, address = address.comb, city = city, state = state,  style = "minimal", output = "sf")
mapview(carjack_sf,
                 map.types = c("OpenStreetMap"),
                 legend = FALSE,
                 popup = popupTable(data,zcol = c("Complaint",
                                                         "Date",
                                                         "Time",
                                                         "Neighborhood",
                                                         "District",
                                                         "Crime",
                                                         "Description")))
##  Mapview shows the 13 carjacking that we not geo mapped.
###########################################################################################
### 14.  To many NA in data  Use carjack complete and leaflet on the 510 observations. This looses 85 out of 510 records
data.one <- mutate(carjack.complete, address.comb = paste(CADAddress, CADStreet, sep = " "), city = "St Louis", state = "MO")
carjack_one.sf <- cxy_geocode(data.one, address = address.comb, city = city, state = state,  style = "minimal", output = "sf")
mapview(carjack_one.sf, map.types = c("OpenStreetMap"),
        legend = FALSE,
        popup = popupTable(data.one, zcol = c("Complaint",
                                                         "Date",
                                                         "Time",
                                                         "Neighborhood",
                                                         "District",
                                                         "Crime",
                                                         "Description")))

## create an sf file that will map coordinates
data.one <- mutate(carjack.complete, address.comb = paste(CADAddress, CADStreet, sep = " "), city = "St Louis", state = "MO")
carjack_one.sf <- st_as_sf(data.one, coords = c("lon", "lat"), crs = 4326, agr = "constant")
view(carjack_one.sf)
STL_Carjacks <- mapview(carjack_one.sf, map.types = c("OpenStreetMap"),
                        legend = FALSE,
                        popup = popupTable(data.one, zcol = c("Complaint",
                                                                   "Date",
                                                                   "Time",
                                                                   "Neighborhood",
                                                                   "District",
                                                                   "Crime",
                                                                   "Description")))


total_carjacks <- STL_Carjacks + carjack_sf 
total_carjacks


###  Add neighborhoods
########Transfer to combined_stl
#####setup Neighborhood shape file for leaflet
#add neighborhood shapes to a data frame
# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
hoods.sf <- readOGR("St Louis Shape files/nbrhds_wards/BND_Nhd88_cw.shp")
hoods.sf <- spTransform(hoods.sf, CRS("+proj=longlat +datum=WGS84"))
hoods <- mapview(hoods.sf, map.types = c("OpenStreetMap"),
                 layer.name = c("Neighborhoods"),
                 alpha.regions = 0.1,
                 alpha = 2,
                 zcol = c("NHD_NAME"))             
# collect neighborhood details from shape file
hoods.df <- as(hoods.sf, "data.frame")
hoods.df
#############################
total_carjacks <- STL_Carjacks + carjack_sf + hoods
total_carjacks
########################################

hoods


##################################
# this works for stl city carjacking data  Need to add hood sf file
z <- leaflet() %>%
  fitBounds(-92, 40, -089, 37) %>%
  setView(-90.24, 38.65, zoom = 12) %>%
  addTiles() %>%
  addMarkers(data = carjack.complete,
                    popup = popupTable(carjack.complete,
                                       zcol = c("Date", "Time", "Neighborhood"),
                                       feature.id = FALSE,
                                       row.numbers = FALSE)) %>%
  addPolygons(data = hoods.df, fill = FALSE, stroke = TRUE, color = "Black", weight = 3)

z
#########################################################################################3
##########################to here########################################

















###  Could probably go away

###################################################################################
###################################################
## 15. look at density data by neighborhood
carjackings <- stl.city.Group2018[, c(1:3,9,10,12,13,14,21,22,23,24)]
carjackings$District = as.factor(carjackings$District)
carjackings$Crime = as.factor(carjackings$Crime)
carjackings$Neighborhood = as.factor(carjackings$Neighborhood)
str(carjackings)
Neighborhd_count <- carjackings %>%
  count(Neighborhood)
###########################################

###  will test join in other script 
##############################
#################################
# join carkacks table with hoods table to get neighborhood names
head(carjackings)
head(hoods.df)
str(carjackings)
str(hoods.df)
hoods.df$NHD_NUM <- as.factor(hoods.df$NHD_NUM)
carjackings_neighborhood <- left_join(carjackings, hoods.df, by = c("Neighborhood" = "NHD_NUM"))

# carjackings_neighborhood <- carjackings_neighborhood[, c(7,8,2,3,4,5,6,9,10)]
head(carjackings_neighborhood)
######################################














############################################
st.louis.carjack <- ggplot(stl.city.Group2018.complete, aes(x = XCoord, YCoord))+
  geom_point()
st.louis.carjack
###################################################

###################################
tibble(st.louis.carjack)
str(carjack.complete)
st.louis.carjack
####################################################
###################################################


###########################################



#  look at plot per month carjacks per neighborhood
Nbrhd <- ggplot(carjackings, aes(x = Neighborhood))+ 
  geom_bar()+
  ggtitle("Carjacks by Neighborhood")
Nbrhd 

#  look at plot per month carjacks per police district
PolDistr <- ggplot(carjackings, aes(x = District))+
  geom_bar()+
  ggtitle("Carjacks by Police District")
PolDistr 


#    group_by(District)
#carjackings
#ungroup(carjackings)

# restrict to downtown
#hijack.area <- carjackings[, c(5:6)]

#available_features()
#q <- getbb("Saint Louis, Missouri")%>%
#  opq()
#str(q) #query structure

#our background map
#stl.map <- get_map(getbb("Saint Louis, Missouri"),maptype = "toner-background")

#final map
#ggmap(stl.map)+
 # geom_sf(data=carjackings,
##          colour="#238443",
#  alpha=.5,
#          size=4,
#          shape=21)+
#  labs(x="",y="")

#stl <- qmap("'St Louis, Missouri", zoom = 14,source = "osm", color = 'bw', legend = 'topleft' )
#stl+geom_point(aes(x = lon, y = lat, 
  #                 size = offense,colour = offense), data = hijack.area )
########################################################
###################################################




##############################
#################################

######################################








#########################Base
# base graphics
ggplot(stl.city.Group2018, aes(x = stl.city.Group2018$District,  col = stl.city.Group2018$Crime)) + geom_histogram()
ggplot(stl.city.Group2018, aes(x = stl.city.Group2018$Neighborhood,  col = stl.city.Group2018$Crime)) + geom_histogram()     
#####################################

####################################### GGPLOT
# ggplot2
ggplot(stl.city.Group2018, aes(x = stl.city.Group2018$XCoord, y = stl.city.Group2018$YCoord, col = stl.city.Group2018$CodedMonth)) + 
  geom_point() + coord_equal()
#####################################

#########################
# break into monthly plots
ggplot(stl.city.Group2018, aes(x = XCoord, y = YCoord, col = CodedMonth)) + geom_point() + facet_wrap(~CodedMonth) + 
  coord_equal()
###############################

###########################
# break into monthly plots
ggplot(stl.city.Group2018, aes(x = XCoord, y = YCoord, col = CodedMonth)) + geom_count()
  


###################################
library(RColorBrewer)
#coordinates(stl.city.Group2018) = ~XCoord + YCoord
#proj4string(stl.city.Group2018) = CRS("+proj=utm +zone=15 ellps=NAD83")
#sp.theme(set = TRUE, regions = list(col = brewer.pal(9, "Set1")))
#spplot(stl.city.Group2018, "CodedMonth")


