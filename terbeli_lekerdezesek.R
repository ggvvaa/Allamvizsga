library(obm)
library(rgdal)
library(rgdal)
library(sp)

OBM_init('transdiptera')
#OBM_init('transdiptera','localhost/biomaps')
tokeRn <- OBM_auth('veres_robi75@yahoo.com', '123456')
t_data <- OBM_get('get_data', '*')
t_data <- data.frame(t_data)
t_data[, 2:15] <- NULL #kivesz egy par oszlopot

la <- unlist(t_data$latitude)
lo <- unlist(t_data$longitude)
sp <- unlist(t_data$species_id)

dat <- data.frame(lon = lo, lat = la, sp = sp)

regi <- readOGR("/home/robi/Allamvizsga/Qgis/Regiune/Regiune.shp", layer = "Regiune")
plot(regi)
points(dat$lat ~ dat$lon, col = "red")

coordinates(dat) <- ~lon + lat #spatial point  dataframe
proj4string(dat) <- proj4string(regi) #atalakitas???

new_shape <- point.in.polygon(dat$lon[4], dat$lat[4], regi@polygons[[3]]@Polygons[[1]]@coords[,1], regi@polygons[[3]]@Polygons[[1]]@coords[,2])

#regi@polygons[[1]]@Polygons[[2]]@coords[,1]

nevek <- regi@data$DENUMIRE