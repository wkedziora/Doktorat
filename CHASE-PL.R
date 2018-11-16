library(raster)
# library(rgdal)
# library(dismo)
# library(rJava)
library(ncdf4)
library(sf)
library(tidyverse)

worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
worldclim <- stack(worldclim_data)

envirem_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\envirem", pattern='\\.tif$', full.names = TRUE)
envirem <- stack(envirem_data)

# precip <- nc_open("D:\\Praca\\Badania\\Doktorat\\data\\CHASE-PL\\PreciForMonths.nc")
# print(precip)
# r_precip <- stack("D:\\Praca\\Badania\\Doktorat\\data\\CHASE-PL\\PreciForMonths.nc", varname = "precipitation_amount")

# gps_coord <- read_tsv("Chris/resultGPS.txt")
gps <- st_as_sf(read_tsv("Chris/resultGPS.txt"), coords = c('long', 'lat'), crs = "+init=epsg:4326")
# gps_coord <- gps_coord[-10927,] #one is out of climatic dataset range
# coordinates(gps_coord) <- ~ long + lat
# proj4string(gps_coord) <- "+init=epsg:4326" #adding WGS84 projection
# set.seed(35)
# gps_coord_sample <- gps_coord[sample(nrow(gps_coord), 10), ]

data <- tibble(gps$SI, raster::extract(worldclim, gps), raster::extract(envirem, gps))
names(data)[1] <- c("SI")

old_data <- data

res <- cor(old_data, use = "complete.obs")
round(res, 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


coordinates(data) <- ~ long + lat
proj4string(data) <- "+init=epsg:4326" #adding WGS84 projection


for (i in names(old_data)[-c(1:3)]) {
plot(old_data[[i]], old_data$SI)
}

linear_model <- lm(SI ~ bio_04 + bio_05 + bio_12, data = data) #u Sochy R = 0,29
summary(linear_model)

library(gam)
gam_model <- gam(SI ~ bio_05 + bio_12, data = data) #u Sochy R = 0,29
summary(gam_model)
