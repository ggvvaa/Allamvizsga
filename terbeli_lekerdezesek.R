library(obm)
library(rgdal)
library(rgdal)
library(sp)
library(readxl)



OBM_init('transdiptera')
tokeRn <- OBM_auth('veres_robi75@yahoo.com', '123456')
t_data <- OBM_get('get_data', '*')
t_data <- data.frame(t_data)
t_data[, 1:8] <- NULL #kivesz egy par oszlopot



regi <- readOGR("/home/robi/Allamvizsga/Qgis/Regiune/Regiune.shp", layer = "Regiune")
tipuloidae <- read_excel("~/Allamvizsga/Fajlista_teljes.xlsx", sheet = "Tipulidae")



plot(regi)
points(dat$lat ~ dat$lon, col = "red")



la <- unlist(t_data$latitude)
lo <- unlist(t_data$longitude)
sp <- unlist(t_data$species_id)
dat <- data.frame(lon = lo, lat = la, sp = sp, fam = NA, subfam = NA, gen = NA, subgen = NA, spec = NA, subspec = NA)



{
  a <- numeric(0)
  for (i in 1:length(dat$sp)) {
    x <- match(dat[i, ]$sp, tipuloidae$taxon_id)
    if (!is.na(x)) {
      dat[i, ]$fam <- tipuloidae[x, ]$family
      dat[i, ]$subfam <- tipuloidae[x, ]$subfamily
      dat[i, ]$gen <- tipuloidae[x, ]$genera
      dat[i, ]$subgen <- tipuloidae[x, ]$subgenera
      dat[i, ]$spec <- tipuloidae[x, ]$species
      dat[i, ]$subspec <- tipuloidae[x, ]$subspecies
    } else {
      a[length(a) + 1] <- i
    }
  }
  dat <- dat[-a,]
} #kiszurjuk a Tipulidae adatokat es beszurjuk a besorolast



coordinates(dat) <- ~lon + lat #spatial point  dataframe
proj4string(dat) <- proj4string(regi) #atalakitas???
new_shape <- point.in.polygon(dat$lon[4], dat$lat[4], regi@polygons[[3]]@Polygons[[1]]@coords[,1], regi@polygons[[3]]@Polygons[[1]]@coords[,2])
#regi@polygons[[1]]@Polygons[[2]]@coords[,1]


nevek <- regi@data$DENUMIRE