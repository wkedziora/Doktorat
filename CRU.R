library(ncdf4)
library(raster)
library(tmap)
library(rgdal)
library(sf)

# loading extent of Poalnd -----
data(World)
Poland <- subset(World, name == "Poland")
wgs.84 <- "+proj=longlat +datum=WGS84"
Poland.wgs84 <- st_transform(Poland, wgs.84)

# temperature -----
# Reference: Harris et al. (2014) doi:10.1002/joc.3711

# MTWA, mean temperature of the warmest month; 
# MTCO, mean temperature of the coldest month; 
# TMAX, absolute maximum temperature;
# TMIN, absolute minimum temperature; 
# MPWE, mean precipitation of the wettest month; 
# MPDR, mean precipitation of the driest month.

tmp_name <- "d://Praca//Badania//Doktorat//data//CRU//cru_ts4.01.1901.2016.tmp.dat.nc"
tmp <- brick(tmp_name, varname="tmp")
tmp_crop <- crop(tmp, extent(Poland.wgs84), snap='out')
# plot-test
plot(tmp_crop[[113]])
plot(Poland.wgs84, border = "black", add = TRUE)

# pdsi -----
# Reference:
# van der Schrier G, Barichivich J, Briffa KR and Jones PD (2013) A scPDSI-based global data set of dry and wet spells for 1901-2009. J. Geophys. Res. Atmos. 118, 4025-4048 (10.1002/jgrd.50355).
# Osborn TJ, Barichivich J, Harris I, van der Schrier G and Jones PD (2017) Monitoring global drought using the self-calibrating Palmer Drought Severity Index [in "State of the Climate in 2016"]. Bulletin of the American Meteorological Society 98, S32-S33 (doi:10.1175/2017BAMSStateoftheClimate.1) (available here).
# Osborn TJ, Barichivich J, Harris I, van der Schrier G and Jones PD (2016) Monitoring global drought using the self-calibrating Palmer Drought Severity Index [in "State of the Climate in 2015"]. Bulletin of the American Meteorological Society 97, S32-S36 (available here).
# Wells N, Goddard S and Hayes MJ (2004) A self-calibrating Palmer Drought Severity Index. Journal of Climate 17, 2335-2351 (doi:10.1175/1520-0442(2004)017<2335:ASPDSI>2.0.CO;2).

pdsi_name <- "d://Praca//Badania//Doktorat//data//CRU//scPDSI.cru.3.25.bams2017.GLOBAL.1901.2016.nc"

pdsi <- brick(pdsi_name, varname="scpdsi")
pdsi_crop <- crop(pdsi, extent(Poland.wgs84), snap='out')
# plot-test
plot(pdsi_crop[[113]])
plot(Poland.wgs84, border = "black", add = TRUE)

# NASA nitrogen data ------
nitro2017_name <- "d://Praca//Badania//Doktorat//data//NASA//OMI_trno2_0.10x0.10_201701_V3.nc"
nitro2014_name <- "d://Praca//Badania//Doktorat//data//NASA//OMI_trno2_0.10x0.10_201405_V3.nc"

nitro2019_name <- "d://Praca//Badania//Doktorat//data//NASA//2019_01_NO2Total.txt"

read.csv(nitro2019_name)

nitro <- stack(nitro2014_name, varname = "TroposphericNO2")
crs(nitro) <- CRS('+init=EPSG:4326')
bb <- extent(-180, 180, -90, 90)
nitro <- setExtent(nitro, bb)
res(nitro) <- 0.1
nitro_crop <- crop(nitro, extent(Poland.wgs84), snap = 'out')
# plot-test
plot(nitro_crop)
plot(Poland.wgs84, border = "black", add = TRUE)