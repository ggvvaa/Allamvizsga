library(dismo)
library(raster)
library(rJava)


#vilag <- readOGR("/home/robi/Qgis/Orszaghatarok/TM_WORLD_BORDERS-0.3.shp")
#eur <- readOGR("/home/robi/Qgis/Europa/Europe_coastline_poly_WGS84.shp")
#ggplot() + 
#  geom_polygon(data = eur, aes(long, lat, group=group), colour='black',fill='white') + 
#  geom_point(data=dat, aes(x=lon, y=lat), color='red',size=2)

files <- list.files("../Maxent/maxent_pelda_grid/", pattern='grd', full.names=TRUE )
predictors <- stack(files)
plot(predictors)
pontok <- dat[dat$sp == 5110, 2:3]
#presvals <- extract(predictors, pontok)
#set.seed(0)
#backgr <- randomPoints(predictors, 5000)
#absvals <- extract(predictors, backgr)
#pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
#sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
#sdmdata[,'ALTA'] = as.factor(sdmdata[,'ALTA'])


xm <- maxent(predictors, pontok, factors = 'ALTA') 
plot(xm)

px <- predict(xm, predictors, progress='')
plot(px)








fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
predictors_c <- stack(fnames)
plot(predictors_c)
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]
me <- maxent(predictors_c, occtrain, factors='biome')
plot(me)
rr <- predict(me, predictors_c, filename='maxent_prediction.tif', overwrite=TRUE)
r <- predict(me, predictors_c)
plot(r)
points(occ)





