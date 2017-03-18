library(obm)
library(rgdal)
library(rgdal)
library(rgeos)
library(sp)
library(readxl)
library(maptools)
library(rgdal)
library(ggplot2)
library(lubridate) #datumok
library(xlsx)
library(plyr) #duplikalt adat szamolas
library(reshape2) #kel a braplot abrazolashoz



#1
{
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
datum <- as.POSIXlt(unlist(t_data$collection_date))
egyed <- unlist(t_data$males) + unlist(t_data$females)
dat <- data.frame(id = id, lon = lo, lat = la, egyed = egyed, datum = datum, fam = NA, subfam = NA, gen = NA, subgen = NA, spec = NA, subspec = NA, auth = NA, sp = sp)
} #kiindulasi adatok lekerdezese



#2
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
      dat[i, ]$auth <- tipuloidae[x, ]$author
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



#3
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



#4
{
  #nPolys <- sapply(regi@polygons, function(x)length(x@Polygons)) #megmondja hogy melyik polygon, hany reszbol all
  regi_f <- fortify(regi)
  nPolys <- unique(regi_f$id) #a kulombozo regioknak az azonositoi, osszesen 16
  regi_fl = list()
  nevek <- numeric(length(nPolys))
  for (i in 1:length(nPolys)) {
    nevek[i] <- unlist(strsplit(as.character(regi$DENUMIRE[i]), ' - '))[2]
  }
  
  
  
  for (i in 1:length(nPolys)) {
    new_shape_1 <- point.in.polygon(dat$lon, dat$lat, regi_f[regi_f$id == nPolys[i],]$long, regi_f[regi_f$id == nPolys[i],]$lat)
    new_shape_2 <- numeric(0)
    for (j in 1:length(new_shape_1)) {
      if (new_shape_1[j] == 0) {
        new_shape_2[length(new_shape_2) + 1] <- j
      }
    }
    fajok <- dat[-new_shape_2,]
    corr <- length(unique(fajok$sp))/((regi$Terulet[i]/1000)^0.15) #a terulet milyen formaba szerepeljen?
    regi_fl[[i]] <- list(adatok = data.frame(nev = nevek[i], 
                                             terulet = regi$Terulet[i], 
                                             telj_fsz = length(unique(fajok$sp)), 
                                             corr_fsz = corr,
                                             gyujtesi_pontok_sz = nrow(unique(fajok[,c('lon', 'lat')])),
                                             egyedszam = sum(fajok$egyed)), 
                         fajok = fajok)
  } 
  #a regi_fl[[1]][[1]] tartalmazza a regio nevet, a regi_fl[[1]][[2]] tartalmazza a regioba talalhato pontokat
  #rei_fl[[i]] az i edik regio informacioi (osszesen 16 van)
  #pl: regi_fl[[2]][[2]][1,] parancsal lehet elerni a masodik listaba levo data frame elemeit
} #regionkent lekerdezzuk a pontokat



#5
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
  ggplot() + 
    scale_x_continuous(name="") +
    scale_y_continuous(name="") +
    geom_polygon(data = rom, aes(long, lat, group=group), colour='black',fill='white') +
    geom_point(data = centroids, aes(x = x, y = y, shape = factor(s), colour = factor(s)), size = 4) + 
    geom_text(data = centroids, vjust = 0, nudge_y = 0.1, aes(x = x, y = y, label = centroids$regiune))
} #regionkenti centroidok meghatarozasa + kategorizalas + kirajzolas



#6
{
  df <- data.frame(matrix(vector(), 0, 6, dimnames = list(c(), c('regio', 'terulet', 'teles_fajszam', 'korrekcios_fajszam', 'gyujtesi_pontok_sz', 'egyedszam'))), stringsAsFactors=F)
  for (i in 1:length(regi_fl)) {
    df[i,]$regio <- nevek[i]
    df[i,]$terulet <- regi_fl[[i]]$adatok$terulet
    df[i,]$teles_fajszam <- regi_fl[[i]]$adatok$telj_fsz
    df[i,]$korrekcios_fajszam <- regi_fl[[i]]$adatok$corr_fsz
    df[i,]$gyujtesi_pontok_sz <- regi_fl[[i]]$adatok$gyujtesi_pontok_sz
    df[i,]$egyedszam <- regi_fl[[i]]$adatok$egyedszam
  }
} #Statisztikahoz elokeszitjuk az adatokat



#8
{
  #regionkenti egyedek lekerdezese honaponkent
  aktivi <- unlist(lapply(regi_fl, function(x) {
                      d <- numeric(12)
                      for (i in 1:length(x$fajok$datum)) {
                        d[month(x$fajok[i,]$datum)] <- d[month(x$fajok[i,]$datum)] + x$fajok[i,]$egyed
                      }
                      return(d)
                    }))
  
  
  
  aktivitas <- matrix(c(aktivi), nrow = length(nevek), byrow = T)
  honapok <- c('jan', 'feb', 'mar', 'apr', 'maj', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec')
  colnames(aktivitas) <- honapok
  rownames(aktivitas) <- nevek
  
  
  
  #honaponkenti gyujtesek szamanak lekerdezese regionkent
  aktivi1 <- unlist(lapply(regi_fl, function(x) {
                      d1 <- unique(x$fajok$datum)
                      d <- numeric(12)
                      for (i in 1:length(d1)) {
                        d[month(d1[i])] <- d[month(d1[i])] + 1
                      }
                      return(d)
                    }))
  
            
  
  aktivitas1 <- matrix(c(aktivi1), nrow = length(nevek), byrow = T)
  honapok <- c('jan', 'feb', 'mar', 'apr', 'maj', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec')
  colnames(aktivitas1) <- honapok
  rownames(aktivitas1) <- nevek 
  
  
  
  #honaponkenti fajok szama regionkent 
  aktivi2 <- unlist(lapply(regi_fl, function(x) {
                      d1 <- unique(x$fajok[,c(5, 13)])
                      d <- numeric(12)
                      for (i in 1:length(d1[,2])) {
                        d[month(d1[i, 1])] <- d[month(d1[i, 1])] + 1
                      }
                      return(d)
                    }))

  
  
  aktivitas2 <- matrix(c(aktivi2), nrow = length(nevek), byrow = T)
  honapok <- c('jan', 'feb', 'mar', 'apr', 'maj', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec')
  colnames(aktivitas2) <- honapok
  rownames(aktivitas2) <- nevek
  
  
  
  #fajok szerinti gyujtese szama regionkent
  #aa <- ddply(regi_fl[[2]]$fajok, .(sp), nrow)
  ef <- unique(dat[,6:13])
  ef_m <- unlist(lapply(regi_fl, function(x) {
    d <- ddply(x$fajok, .(sp), nrow)
    d1 <- matrix(rep(0, nrow(ef)), nrow = 1)
    colnames(d1) <- ef$sp
    if (length(d$sp) != 0) {
      for (i in 1:length(d$sp)) {
        d1[1,paste(d$sp[i])] <- d$V1[i]
      }
    }
    return(d1)
  }))
  
  
  
  ef_m <- matrix(ef_m, ncol = length(ef$sp), byrow = T)
  colnames(ef_m) <- paste(ef$fam, ' ', ef$subfam, ' ', ef$gen, ' ', ef$subgen, ' ', ef$spec, ' ', ef$subspec)
  rownames(ef_m) <- nevek
} #tabalzatok lekerdezesek



#7
{
  for (i in 1:length(nPolys)) {
    if (regi_fl[[i]]$adatok$telj_fsz != 0) {
      write.xlsx(regi_fl[[i]]$fajok, file = "tipulidae_eredmenyek.xlsx", sheetName = df[i,]$regio, append = TRUE, showNA = F)
    }
  }
  write.xlsx(df, file = "tipulidae_eredmenyek.xlsx", sheetName = 'Regionkenti adatok', append = TRUE, showNA = F)
  write.xlsx(aktivitas, file = "tipulidae_eredmenyek.xlsx", sheetName = 'Honaponkent gyujtott egyedek', append = TRUE, showNA = F)
  write.xlsx(aktivitas1, file = "tipulidae_eredmenyek.xlsx", sheetName = 'Honaponkenti gyujtesi alkalmak', append = TRUE, showNA = F)
  write.xlsx(aktivitas2, file = "tipulidae_eredmenyek.xlsx", sheetName = 'Honaponkenti befogott fajok', append = TRUE, showNA = F)
  write.xlsx(ef_m, file = "tipulidae_eredmenyek.xlsx", sheetName = 'Fajok szerinti gyujtese szama', append = TRUE, showNA = F)
} #exportalas



#9
{
  #pontok regionkenti kirajzolasa
  for (i in 1:length(regi_fl)) {
    print(
      ggplot() + 
        scale_x_continuous(name="") +
        scale_y_continuous(name="") +
        geom_polygon(data = regi, aes(long, lat, group = group), colour = 'black', fill = 'white') + 
        geom_point(data = regi_fl[[i]][[2]],  aes(x = lon, y = lat), color = 'red', size = 2) + 
        ggtitle(regi_fl[[i]]$adatok$nev)
    )
  } 
  
  
  
  #romaniaban talalhato pntok kirajzolasa
  ggplot() + 
    geom_polygon(data = regi, aes(long, lat, group=group), colour='black',fill='white') + 
    geom_point(data=dat, aes(x=lon, y=lat), color='red',size=2)
  
  
  
  #honaponkenti gyujtesek regionkenti kirajzolasa
  r_aktivitas <- melt(aktivitas)
  su <- matrix(as.integer(colSums(aktivitas)), nrow = 1)
  colnames(su) <- honapok
  r_su <- melt(su)
  
  
  
  ggplot() + 
    geom_bar(data = r_aktivitas, aes(x = Var2, y = value, fill = factor(Var1)), stat = "identity", position="dodge", width=1) +
    geom_line(data = r_su, aes(x = Var2, y = value, group = Var1, color = factor(Var2))) + 
    guides(colour=FALSE)
} #plot - oks