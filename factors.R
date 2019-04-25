############################################
### Factors preparation for PhD research ###
############################################

library(raster)
library(sf)
library(tidyverse)
library(feather)

#WorldClim data is used in CliMond set so I am rejecting it from analysis, however, they are in finer resolution
# worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
# worldclim <- stack(worldclim_data)

climond_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\CliMond", pattern='\\.tif$', full.names = TRUE)
climond <- stack(climond_data[1:35])

envirem_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\envirem", pattern='\\.tif$', full.names = TRUE)
envirem <- stack(envirem_data)

# distances and angles to closests (1) lakes and water bodies, (2) big rivers, (3) medium rivers
water_dist <- read_csv2("shp/distance/distance.txt") %>% dplyr::select(-c("FID", "SZEROKOSC", "DLUGOSC", "ROK_W_CYKL"))

# gps_coord <- read_tsv("Chris/resultGPS.txt")
si <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather")) %>% 
        left_join(., water_dist, by = c("nr_punktu" = "NR_PUNKTU")) %>% 
        dplyr::select(SI, kraina, wiek_pan_pr, b_pion_pow_pr, tsl, zyzn, wilg1, wilg2, szerokosc, dlugosc, gp, wlasc,
                      n_drzew, n_gat, SDI, jez_dist, drzek_dist, rzeki_dist, woda_dist)
# gps <- st_as_sf(read_tsv("Chris/resultGPS.txt"), coords = c('long', 'lat'), crs = "+init=epsg:4326")
# si <- si[-10927,] #one is out of climatic dataset range
si_xy <- si
coordinates(si_xy) <- ~ dlugosc + szerokosc
proj4string(si_xy) <- "+init=epsg:4326" #adding WGS84 projection

# data extraction
si_factors <- as_tibble(data.frame(si_xy@data, coordinates(si_xy), raster::extract(climond, si_xy), raster::extract(envirem, si_xy)))
summary(si_factors) # NA's are out of raster bounds

# write_feather(si_factors, paste0(getwd(), "/data/WISL/si_factors.feather"))
# si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather"))
