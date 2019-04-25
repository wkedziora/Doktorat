### GW Local Statistics for PhD research ###

library(tidyverse)
library(tmap)
library(sf)
library(sp)
library(RColorBrewer)
library(feather)
library(GWmodel)

source("geom_marginboxplot.R") # loading user functions

# data loading -------------------------------------------------------------------------------------------------
site_index <- read_feather(paste0(getwd(), "/data/WISL/site_index.feather"))
site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

coordinates(site_index_gps) <- ~ dlugosc + szerokosc #adding sptial relationship
proj4string(site_index_gps) <- "+init=epsg:4326" #adding WGS84 projection
# site_index_gps <- st_as_sf(site_index_gps, coords = c('dlugosc', 'szerokosc'), crs = "+init=epsg:4326")

# site index map plotting -------------------------------------------------------------------------------------------------
st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "Region") %>% filter(COUNTRY == "Poland") -> Poland.shp

Poland.shp <- as(Poland.shp, "Spatial")
krainy.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")

map_SI <- plot_map_dots(site_index_gps, "SI")
tmap_save(map_SI, filename = "mapy/map_SI.png", dpi = 600, width = 15, height = 15, units = "cm")

plot_map_dots(site_index_gps[which(site_index_gps$SI < 10),], "SI")
plot_map_dots(site_index_gps[which(site_index_gps$SI < 15),], "SI")
plot_map_dots(site_index_gps[which(site_index_gps$SI > 35),], "SI")
plot_map_dots(site_index_gps[which(site_index_gps$SI > 40),], "SI")

# trzy najczęstsze siedliska
tmap_save(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "Bśw"),], "SI"),
          filename = "mapy/map_SI_Bsw.png", dpi = 600, width = 15, height = 15, units = "cm")

tmap_save(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "BMśw"),], "SI"), 
          filename = "mapy/map_SI_BMsw.png", dpi = 600, width = 15, height = 15, units = "cm")

tmap_save(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "LMśw"),], "SI"), 
          filename = "mapy/map_SI_LMsw.png", dpi = 600, width = 15, height = 15, units = "cm")


### local Moran's I ---------------------------------------------------------------------------------------------------
library(spatstat)
library(spdep)
library(maptools)
# library(parallel)
# library(doParallel)
# 
# nc <- detectCores(logical=FALSE) # Calculate the number of cores
# cl <- makeCluster(nc) # Initiate cluster
# registerDoParallel(cl)
# 
# set.coresOption(as.integer(4))
# set.ClusterOption(cl)

site_index_moran <- site_index_gps

nghbr_1 <- dnearneigh(site_index_moran, 0, 1, longlat = TRUE)
moran_1 <- localmoran(site_index_moran$SI, nb2listw(nghbr_1, zero.policy = TRUE), na.action = na.omit)

nghbr_5 <- dnearneigh(site_index_moran, 0, 5, longlat = TRUE)
moran_5 <- localmoran(site_index_moran$SI, nb2listw(nghbr_5, zero.policy = TRUE), na.action = na.omit)

nghbr_10 <- dnearneigh(site_index_moran, 0, 10, longlat = TRUE)
moran_10 <- localmoran(site_index_moran$SI, nb2listw(nghbr_10, zero.policy = TRUE), na.action = na.omit)

nghbr_25 <- dnearneigh(site_index_moran, 0, 25, longlat = TRUE)
moran_25 <- localmoran(site_index_moran$SI, nb2listw(nghbr_25, zero.policy = TRUE), na.action = na.omit)

nghbr_50 <- dnearneigh(site_index_moran, 0, 50, longlat = TRUE)
moran_50 <- localmoran(site_index_moran$SI, nb2listw(nghbr_50, zero.policy = TRUE), na.action = na.omit)

nghbr_100 <- dnearneigh(site_index_moran, 0, 100, longlat = TRUE)
moran_100 <- localmoran(site_index_moran$SI, nb2listw(nghbr_100, zero.policy = TRUE), na.action = na.omit)

site_index_moran@data <- data.frame(site_index_moran@data, 
                                    as.data.frame(moran_1), 
                                    as.data.frame(moran_5),
                                    as.data.frame(moran_10), 
                                    as.data.frame(moran_25),
                                    as.data.frame(moran_50),
                                    as.data.frame(moran_100))

plot_map_dots(site_index_moran, "SI")
plot_map_dots(site_index_moran, "z_mean")
plot_map_dots(site_index_moran, "z_sd")
plot_map_dots(site_index_moran, "Ii")
plot_map_dots(site_index_moran, "Ii.1")
plot_map_dots(site_index_moran, "Ii.2")
plot_map_dots(site_index_moran, "Ii.3")
plot_map_dots(site_index_moran, "Ii.4")
plot_map_dots(site_index_moran, "Ii.5")

### GW Summary Statistics -------------------------------------------------------------------------------------------------

# data(Europe, rivers)
# Poland <- subset(Europe, name == "Poland")
# Poland_2180 <- spTransform(Poland, "+init=epsg:2180")

gwss_dist <- gw.dist(dp.locat = coordinates(si_factors_cc_2180), longlat = FALSE)

gwss_bw <- bw.gwss.average(si_factors_cc_2180, vars = c("SI"), kernel = "bisquare", adaptive = FALSE,
                p = 2, theta = 0, longlat = FALSE, dMat = gwss_dist) # 740397 ???


plot_map_dots(site_index_gps, "SI") #test
tmap_save(plot_map_dots(site_index_gps, "SI"), filename = "mapy/map_SI.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat6 <- gwss(site_index_gps, vars = c("SI"), bw = 6.2, longlat = TRUE)
tmap_save(plot_map_dots(localstat6$SDF, "SI_LM", breaks = c(-Inf, 10, 14, 18, 22, 26, 30, 34, Inf)),
          map_gwss_006km, filename = "mapy/map_gwss_006km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat10 <- gwss(site_index_gps, vars = c("SI"), bw = 10, longlat = TRUE)
tmap_save(plot_map_dots(localstat10$SDF, "SI_LM", breaks = seq(20, 36, 2)),
          map_gwss_010km, filename = "mapy/map_gwss_010km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat25 <- gwss(site_index_gps, vars = c("SI"), bw = 25, longlat = TRUE)
tmap_save(plot_map_dots(localstat25$SDF, "SI_LM", breaks = seq(20, 36, 2)), 
          map_gwss_025km, filename = "mapy/map_gwss_025km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat50 <- gwss(site_index_gps, vars = c("SI"), bw = 50, longlat = TRUE)
tmap_save(plot_map_dots(localstat50$SDF, "SI_LM", breaks = seq(20, 34, 2)), 
          map_gwss_050km, filename = "mapy/map_gwss_050km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat100 <- gwss(site_index_gps, vars = c("SI"), bw = 100, longlat = TRUE)
tmap_save(plot_map_dots(localstat100$SDF, "SI_LM", breaks = seq(25, 33, 1)), 
          map_gwss_100km, filename = "mapy/map_gwss_100km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat200 <- gwss(site_index_gps, vars = c("SI"), bw = 200, longlat = TRUE)
tmap_save(plot_map_dots(localstat200$SDF, "SI_LM", breaks = seq(26, 31, 1)), 
          map_gwss_200km, filename = "mapy/map_gwss_200km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat300 <- gwss(site_index_gps, vars = c("SI"), bw = 300, longlat = TRUE)
tmap_save(plot_map_dots(localstat300$SDF, "SI_LM", breaks = seq(26, 30, 1)), 
          map_gwss_300km, filename = "mapy/map_gwss_300km.png", dpi = 600, width = 15, height = 15, units = "cm")

bw.gwss.average(site_index_gps, site_index_gps, vars = c("SI"), kernel = "bisquare", longlat = T)

### gwss w ujęciu grup żyzności siedlisk ---------------------------------------------------------------------------

localstat50B <- gwss(site_index_gps[which(site_index_gps@data$zyzn == "B"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50B$SDF, "SI_LM")
localstat50BM <- gwss(site_index_gps[which(site_index_gps@data$zyzn == "BM"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50BM$SDF, "SI_LM")
localstat50LM <- gwss(site_index_gps[which(site_index_gps@data$zyzn == "LM"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50LM$SDF, "SI_LM")
localstat50L <- gwss(site_index_gps[which(site_index_gps@data$zyzn == "L"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50L$SDF, "SI_LM")

### gwss w ujęciu grup wilgotności siedlisk ----------------------------------------------------------

localstat50s <- gwss(site_index_gps[which(site_index_gps@data$wilg1 == "s"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50s$SDF, "SI_LM")
localstat50sw <- gwss(site_index_gps[which(site_index_gps@data$wilg1 == "św"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50sw$SDF, "SI_LM")
localstat50w <- gwss(site_index_gps[which(site_index_gps@data$wilg1 == "w"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50w$SDF, "SI_LM")
localstat50b <- gwss(site_index_gps[which(site_index_gps@data$wilg1 == "b"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50b$SDF, "SI_LM")
localstat50z <- gwss(site_index_gps[which(site_index_gps@data$wilg1 == "z"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50z$SDF, "SI_LM")

### gwss w ujęciu pojedynczych siedlisk --------------------------------------------------------------------

localstat25Bsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Bśw"),], vars = c("SI"), bw = 25, longlat = TRUE)
map_25_Bsw <- plot_map_dots(localstat25Bsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))

localstat50Bsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Bśw"),], vars = c("SI"), bw = 50, longlat = TRUE)
map_50_Bsw <- plot_map_dots(localstat50Bsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))
tmap_save(map_50_Bsw, filename = "mapy/map_gwss_Bsw_50km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat25BMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw"),], vars = c("SI"), bw = 25, longlat = TRUE)
map_25_BMsw <- plot_map_dots(localstat25BMsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))

localstat50BMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw"),], vars = c("SI"), bw = 50, longlat = TRUE)
map_50_BMsw <- plot_map_dots(localstat50BMsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))
tmap_save(map_50_BMsw, filename = "mapy/map_gwss_BMsw_50km.png", dpi = 600, width = 15, height = 15, units = "cm")

localstat25LMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw"),], vars = c("SI"), bw = 25, longlat = TRUE)
map_25_LMsw <- plot_map_dots(localstat25LMsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))

localstat50LMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw"),], vars = c("SI"), bw = 50, longlat = TRUE)
map_50_LMsw <- plot_map_dots(localstat50LMsw$SDF, "SI_LM", breaks = c(-Inf, 14, 18, 22, 26, 30, Inf))
tmap_save(map_50_LMsw, filename = "mapy/map_gwss_LMsw_50km.png", dpi = 600, width = 15, height = 15, units = "cm")


localstat10Bw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Bw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10Bw$SDF, "SI_LM")
localstat50Bw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Bw"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50Bw$SDF, "SI_LM")

localstat10BMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10BMsw$SDF, "SI_LM")


localstat10BMw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10BMw$SDF, "SI_LM")
localstat50BMw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMw"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50BMw$SDF, "SI_LM")

localstat10LMsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10LMsw$SDF, "SI_LM")


localstat10LMw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10LMw$SDF, "SI_LM")
localstat50LMw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMw"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50LMw$SDF, "SI_LM")

localstat10Lsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Lśw"),], vars = c("SI"), bw = 10, longlat = TRUE)
plot_map_dots(localstat10Lsw$SDF, "SI_LM")
localstat50Lsw <- gwss(site_index_gps[which(site_index_gps@data$tsl == "Lśw"),], vars = c("SI"), bw = 50, longlat = TRUE)
plot_map_dots(localstat50Lsw$SDF, "SI_LM")

tmap_arrange(plot_map_dots(localstat50Bsw$SDF, "SI_LM"), plot_map_dots(localstat50Bw$SDF, "SI_LM"))
tmap_arrange(plot_map_dots(localstat50BMsw$SDF, "SI_LM"), plot_map_dots(localstat50BMw$SDF, "SI_LM"))
tmap_arrange(plot_map_dots(localstat50LMsw$SDF, "SI_LM"), plot_map_dots(localstat50LMw$SDF, "SI_LM"))

### wyniki vs. wygladzenie 50 km -------------------------------------------------------------------------------------------------

Bsw <- tmap_arrange(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "Bśw"),], "SI"), plot_map_dots(localstat50Bsw$SDF, "SI_LM"))
tmap_save(Bsw, filename = "mapy/map_double_Bsw.png", dpi = 600, width = 18, height = 10, units = "cm")

BMsw <- tmap_arrange(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "BMśw"),], "SI"), plot_map_dots(localstat50BMsw$SDF, "SI_LM"))
tmap_save(BMsw, filename = "mapy/map_double_BMsw.png", dpi = 600, width = 18, height = 10, units = "cm")

LMsw <- tmap_arrange(plot_map_dots(site_index_gps[which(site_index_gps$tsl == "LMśw"),], "SI"), plot_map_dots(localstat50LMsw$SDF, "SI_LM"))
tmap_save(LMsw, filename = "mapy/map_double_LMsw.png", dpi = 600, width = 18, height = 10, units = "cm")


# ### analiza w podziale na klasy wieku i porównanie międzysiedliskowe ------------------------------------
# localstat50BMswI <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "I"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswI <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "I"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswI$SDF, "SI_LM"), plot_map_dots(localstat50LMswI$SDF, "SI_LM"))
# 
# localstat50BMswII <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "II"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswII <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "II"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswII$SDF, "SI_LM"), plot_map_dots(localstat50LMswII$SDF, "SI_LM"))
# 
# localstat50BMswIII <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "III"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswIII <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "III"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswIII$SDF, "SI_LM"), plot_map_dots(localstat50LMswIII$SDF, "SI_LM"))
# 
# localstat50BMswIV <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "IV"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswIV <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "IV"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswIV$SDF, "SI_LM"), plot_map_dots(localstat50LMswIV$SDF, "SI_LM"))
# 
# localstat50BMswV <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "V"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswV <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "V"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswV$SDF, "SI_LM"), plot_map_dots(localstat50LMswV$SDF, "SI_LM"))
# 
# localstat50BMswVI <- gwss(site_index_gps[which(site_index_gps@data$tsl == "BMśw" & site_index_gps@data$kw == "VI i st"),], vars = c("SI"), bw = 50, longlat = TRUE)
# localstat50LMswV <- gwss(site_index_gps[which(site_index_gps@data$tsl == "LMśw" & site_index_gps@data$kw == "VI i st"),], vars = c("SI"), bw = 50, longlat = TRUE)
# tmap_arrange(plot_map_dots(localstat50BMswV$SDF, "SI_LM"), plot_map_dots(localstat50LMswV$SDF, "SI_LM"))

# ### saving data for shiny GW Local Statistics ----------------------------------------------------------------------------
# saveRDS(localstat1, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat1.rds")
# saveRDS(localstat6, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat6.rds")
# saveRDS(localstat10, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat10.rds")
# saveRDS(localstat25, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat25.rds")
# saveRDS(localstat50, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat50.rds")
# saveRDS(localstat100, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat100.rds")
# saveRDS(localstat200, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat200.rds")
# saveRDS(localstat300, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat300.rds")
# saveRDS(localstat400, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat400.rds")
# saveRDS(localstat500, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat500.rds")
# saveRDS(Poland.shp, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/Poland.shp.rds")
# localstat1 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat1.rds")
# localstat6 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat6.rds")
# localstat10 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat10.rds")
# localstat25 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat25.rds")
# localstat50 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat50.rds")
# localstat100 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat100.rds")
# localstat200 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat200.rds")
# localstat300 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat300.rds")
# localstat400 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat400.rds")
# localstat500 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat500.rds")
# Poland.shp <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/Poland.shp.rds")

### old quick.map function -----
# quick.map <- function(spdf, var, legend.title, color) {
#   x <- unlist(spdf@data[,var])
#   cut.vals <- pretty(x)
#   x.cut <- cut(x, cut.vals)
#   cut.levels <- levels(x.cut)
#   cut.band <- match(x.cut, cut.levels)
#   colors <- rev(brewer.pal(length(cut.levels), color))
#   par(mar = c(1, 1, 1, 1))
#   plot(Poland.shp)
#   # title(main.title)
#   plot(spdf, add = T, col = colors[cut.band], pch = 20)
#   legend('bottomleft', cut.levels, col = colors, pch = 20, bty = 'n', title = legend.title)
# }