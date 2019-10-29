##  This shows the area of the police emphasis



###  Task force focus  (Put in presentation early)
###################################################################
## Created database that defines the crime focus area
police_crime_focus <- fread("C:/users/jd1/Desktop/ALL R Stuff and Research/Test_data_table/police_crime_focus.csv", stringsAsFactors=FALSE)
## Create a spatial file of the police crime focus
police_crime_focus
police_point.sf <- st_as_sf(police_crime_focus,
                            coords = c("lon", "lat"),
                            crs = 4326, agr = "constant")
##police points
police_point.sf
## Create matrisx of lat/long
df <- data.frame(police_crime_focus$lon, police_crime_focus$lat)
# You need first to close your polygon 
# (first and last points must be identical)
df <- rbind(df, df[1,])
## Create a lolygon of the area of the police box
police.polygon <- st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = 4326)
police.polygon
police.box <- mapview(police.polygon, map.types = c("OpenStreetMap"),
                layer.name = c("Police Box"),
                alpha.regions = 0.1,
                alpha = 1,
                label = NULL,
                color = "red",
                col.regions = "red")
## Show police box in red
police.box 

 ## Add in Police Box               
STLtotal_carjacks <- STL_Carjacks + carjack_sf + hoods + police.box 
STLtotal_carjacks      
###################################################################################
