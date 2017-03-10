library(obm)
library(rgdal)
library(rgdal)
library(sp)
library(readxl)



OBM_init('transdiptera')
tokeRn <- OBM_auth('veres_robi75@yahoo.com', '123456')
t_data <- OBM_get('get_data', '*')
t_data <- data.frame(t_data)
t_data[, 2:8] <- NULL #kivesz egy par oszlopot



regi <- readOGR("/home/robi/Allamvizsga/Qgis/Regiune/Regiune.shp", layer = "Regiune")
rom <- readOGR("/home/robi/Allamvizsga/Qgis/Subregiune/Romania_teljes.shp", layer = "Romania_teljes")
tipuloidae <- read_excel("~/Allamvizsga/Fajlista_teljes.xlsx", sheet = "Tipulidae")



id <- unlist(t_data$obm_id)
la <- unlist(t_data$latitude)
lo <- unlist(t_data$longitude)
sp <- unlist(t_data$species_id)
dat <- data.frame(id = id, lon = lo, lat = la, fam = NA, subfam = NA, gen = NA, subgen = NA, spec = NA, subspec = NA, sp = sp)



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



# b <- matrix(c(1:25), nrow = 5)
# apply(b, 1, function(i, q)
#   i[1] > q, q = 1)
# sapply(b[, 2:3], function(i)
#   i[2] > i[3])
#coordinates(dat) <- ~lon + lat #spatial point  dataframe
#proj4string(dat) <- proj4string(rom) #atalakitas???
#regi@polygons[[1]]@Polygons[[2]]@coords[,1]


{
  new_shape_1 <-
    point.in.polygon(
      dat$lon,
      dat$lat,
      rom@polygons[[1]]@Polygons[[1]]@coords[, 1],
      rom@polygons[[1]]@Polygons[[1]]@coords[, 2]
    )
  
  a <- numeric(0)
  
  for (i in 1:length(new_shape_1)) {
    if (new_shape_1[i] == 0) {
      a[length(a) + 1] = i
    }
  }
  
  dat <- dat[-a, ]
} #kiszurjuk azokat az adatokat, amelyek Romaniaba vannak



plot(regi)
points(dat$lat ~ dat$lon, col = "red")



nevek <- regi@data$DENUMIRE