library(ncdf4)
library(raster)
library(tmap)
library(rgdal)

# MTWA, mean temperature of the warmest month; 
# MTCO, mean temperature of the coldest month; 
# TMAX, absolute maximum temperature;
# TMIN, absolute minimum temperature; 
# MPWE, mean precipitation of the wettest month; 
# MPDR, mean precipitation of the driest month.

ncname <- "d://Praca//Badania//Doktorat//CRU//cru_ts4.01.2011.2016.tmp.dat.nc"
dname <- "tmp"  # note: tmp means temperature (not temporary)
ncin <- nc_open(ncname)

b <- brick(ncname, varname="tmp")
Poland <- subset(Europe, name == "Poland")

wgs.84       <- "+proj=longlat +datum=WGS84"
Poland.wgs84    <- spTransform(Poland, CRS(wgs.84))

plot(b[[1]])
plot(Poland.wgs84, col="yellow", add = TRUE)

crop <- crop(b, extent(Poland.wgs84), snap='out')

plot(crop[[1]])
plot(Poland.wgs84, col="yellow", add = TRUE)

#
