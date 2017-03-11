library(obm)
library(rgdal)
library(rgdal)
library(rgeos)
library(sp)
library(readxl)
library(maptools)
library(rgdal)
library(ggplot2)



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



# plot(regi)
# points(dat$lat ~ dat$lon, col = "red")
ggplot() + geom_polygon(data = regi, aes(long, lat, group=group), colour='black',fill='white') + geom_point(data=dat, aes(x=lon, y=lat), color='red',size=2)



{
  #nPolys <- sapply(regi@polygons, function(x)length(x@Polygons)) #megmondja hogy melyik polygon, hany reszbol all
  regi_f <- fortify(regi)
  nPolys <- unique(regi_f$id) #a kulombozo regioknak az azonositoi, osszesen 16
  regi_fl = list()
  nevek <- as.character(regi$DENUMIRE)
  
  
  for (i in 1:length(nPolys)) {
    new_shape_1 <- point.in.polygon(dat$lon, dat$lat, regi_f[regi_f$id == nPolys[i],]$long, regi_f[regi_f$id == nPolys[i],]$lat)
    new_shape_2 <- numeric(0)
    for (j in 1:length(new_shape_1)) {
      if (new_shape_1[j] == 0) {
        new_shape_2[length(new_shape_2) + 1] <- j
      }
    }
    regi_fl[[i]] <- list(nevek[i], dat[-new_shape_2,])
  } 
  #a regi_fl[[1]][[1]] tartalmazza a regio nevet, a regi_fl[[1]][[2]] tartalmazza a regioba talalhato pontokat
  #rei_fl[[i]] az i edik regio informacioi (osszesen 16 van)
  #pl: regi_fl[[2]][[2]][1,] parancsal lehet elerni a masodik listaba levo data frame elemeit
} #regionkent lekerdezzuk a pontokat


  
for (i in 1:length(regi_fl)) {
  print(
    ggplot() + geom_polygon(
      data = regi,
      aes(long, lat, group = group),
      colour = 'black',
      fill = 'white'
    ) + geom_point(
      data = regi_fl[[i]][[2]],
      aes(x = lon, y = lat),
      color = 'red',
      size = 2
    ) + ggtitle(regi_fl[[i]][[1]])
  )
} #pontok regionkent



{
  centroids <- data.frame(gCentroid(regi, byid = TRUE))
  centroids$regiune <- regi$REGIUNE
  s <-
    c(
      'podis' ,
      'campie',
      'carpati',
      'depresiune',
      'dealuri',
      'carpati',
      'dealuri',
      'carpati',
      'carpati',
      'carpati',
      'dealuri',
      'campie',
      'podis',
      'podis',
      'campie',
      'carpati'
    )
  ggplot(data = centroids, aes(
    x = x,
    y = y,
    label = centroids$regiune
  )) + geom_point() + geom_text(vjust = 0, nudge_y = 0.1, aes(colour = factor(s)))
} #regionkenti centroidok meghatarozasa + kategorizalas