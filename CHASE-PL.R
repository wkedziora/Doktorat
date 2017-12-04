library(raster)
library(rgdal)
library(dismo)
# library(rJava)
library(ncdf4)
library(tidyverse)

worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
worldclim <- stack(worldclim_data)
plot(worldclim[[5]])

gps_coord <- read_tsv("Chris/resultGPS.txt")
gps_coord <- gps_coord[-10927,] #one is out of climatic dataset range
coordinates(gps_coord) <- ~ long + lat
proj4string(gps_coord) <- "+init=epsg:4326" #adding WGS84 projection
# set.seed(35)
# gps_coord_sample <- gps_coord[sample(nrow(gps_coord), 10), ]

data <- data.frame(coordinates(gps_coord),
                   gps_coord$SI, 
                   raster::extract(worldclim, gps_coord))
names(data)[3] <- c("SI")

coordinates(data) <- ~ long + lat
proj4string(data) <- "+init=epsg:4326" #adding WGS84 projection

linear_model <- lm(SI ~ ., data = data)
summary(linear_model)

g_linear_model <- glm(SI ~ ., data = data)
summary(g_linear_model)
