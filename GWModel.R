########################################
### GWModel setting for PhD research ###
########################################

library(tidyverse)
library(tmap)
# library(rgdal)
library(sf)
# library(FSA)
library(RColorBrewer)
# library(EnvStats)
library(feather)
library(GWmodel)

source("geom_marginboxplot.R") # loading user functions

# data loading -----
site_index <- read_feather(paste0(getwd(), "/data/WISL/site_index.feather")) %>% droplevels()
site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather")) %>% droplevels()
si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather")) %>% droplevels()

coordinates(site_index_gps) <- ~ dlugosc + szerokosc #adding sptial relationship
proj4string(site_index_gps) <- "+init=epsg:4326" #adding WGS84 projection
# site_index_gps <- st_as_sf(site_index_gps, coords = c('dlugosc', 'szerokosc'), crs = "+init=epsg:4326")

# plot_map_dots(site_index_gps, "SI")

rf_model <- readRDS(file = "D:/Praca/Badania/Doktorat/model/rf_model.rds")

### residuals --------------------------------------------------------------------------------
summary(si_factors)

si_factors_cc <- si_factors[complete.cases(si_factors$bio01) & complete.cases(si_factors$tri), ]
# coordinates(si_factors_cc) <- ~ dlugosc + szerokosc
# proj4string(si_factors_cc) <- "+init=epsg:4326" #adding WGS84 projection
si_factors_cc <- st_as_sf(si_factors_cc, coords = c('dlugosc', 'szerokosc'), crs = "+init=epsg:4326", remove = FALSE)

# si_factors_cc_2180 <- spTransform(si_factors_cc, "+init=epsg:2180")
si_factors_cc_2180 <- st_transform(si_factors_cc, "+init=epsg:2180")

si_factors_cc_2180$rf_predict <- predict(rf_model, si_factors_cc_2180)
plot(si_factors_cc_2180$SI, si_factors_cc_2180$rf_predict)

si_factors_cc_2180 %>% mutate(rf_resid = rf_predict - SI) -> si_factors_cc_2180
plot(si_factors_cc_2180$SI, si_factors_cc_2180$rf_resid)
    
plot_map_dots(si_factors_cc_2180, "rf_resid", breaks = c(-10, 0, 10, 20))

### Moran's I ---------------------------------------------------------------------------------------------------
library(ape)

resid_dists_inv <- 1/as.matrix(dist(st_coordinates(si_factors_cc_2180)))
# resid_dists_inv <- 1/resid_dists
diag(resid_dists_inv) <- 0
resid_dists_inv[is.infinite(resid_dists_inv)] <- 0

Moran.I(si_factors_cc_2180$rf_resid, resid_dists_inv)

### If there is sptial correlation of residuals I have right to start GWModel 
# - it is !!! 
# $observed 0.009289052
# 
# $expected -6.459531e-05 -> -0.00006459531
#         
# $p.value 0 !!!

### GWmodel ---------------------------------------------------------------------------------------------------------------
si_factors_cc_2180 <- as(si_factors_cc_2180, "Spatial")

model_formula <- as.formula(SI ~ wiek_pan_pr + b_pion_pow_pr + zyzn + wilg1 + 
                                    wlasc + n_drzew + n_gat + SDI + jez_dist + drzek_dist + rzeki_dist + 
                                    dlugosc + bio21 + bio24 + bio25 + bio27 + bio35 + aridityIndexThornthwaite + 
                                    growingDegDays0 + maxTempColdest + minTempWarmest + monthCountByTemp10 + 
                                    PETDriestQuarter + PETWettestQuarter + tri)

dist <- gw.dist(dp.locat = coordinates(si_factors_cc_2180))

bandwidth_fixed <- bw.gwr(model_formula, data = si_factors_cc_2180, kernel = "bisquare", approach = "AICc") 
# bisquare: new 329503 old 8202.084
bandwidth_fixed <- 329503

bandwidth_fixed_cv <- bw.gwr(model_formula, data = si_factors_cc_2180, kernel = "bisquare", approach = "CV") 
# bisquare: new 537479 old 7280.522
bandwidth_fixed_cv <- 537479

bandwidth_adaptive <- bw.gwr(model_formula, data = si_factors_cc_2180, kernel = "bisquare", approach = "AICc", adaptive = T, dMat = dist) 
# bisquare: new -7599- old 16
bandwidth_adaptive <- 7599

bandwidth_adaptive_CV <- bw.gwr(model_formula, data = si_factors_cc_2180, kernel = "bisquare", approach = "CV", adaptive = T, dMat = dist) 
# bisquare: new -755- old 16
bandwidth_adaptive_CV <- 755
        
bandwidth <- 329503

# gwr_collin_diag <- gwr.collin.diagno(model_formula, 
#                                      data = si_factors_cc_2180, 
#                                      bw = bandwidth,
#                                      dMat = dist, 
#                                      kernel = 'bisquare')
# saveRDS(gwr_collin_diag, paste0(getwd(), "/data/WISL/gwr_collin_diag.rds"), version = 3)
gwr_collin_diag <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_collin_diag.rds")

# gwr_bio01 <- gwr.basic(SI ~ bio01, data = si_factors_gw_2180, bw = bandwidth, kernel = "bisquare", dMat = dist) # 0.002 -> 0.261

gwr_vipaic_model_bw1 <- gwr.basic(model_formula, 
                                  data = si_factors_cc_2180, 
                                  bw = bandwidth,
                                  dMat = dist, 
                                  kernel = 'gaussian')

# saveRDS(gwr_vipaic_model_bw2, paste0(getwd(), "/data/WISL/gwr_vipaic_model_bw2.rds"), version = 3)
# gwr_1 <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_1.rds")
gwr_vipaic_model_bw2 <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_vipaic_model_bw2.rds")
# vipaic_model: global regression Rsqadj = 0,563 GW regression Rsq = 0,756 Rsqadj = 1

gwr_vipaic_model_bw10km <- gwr.basic(model_formula, 
                                  data = si_factors_cc_2180, 
                                  bw = 10000,
                                  dMat = dist, 
                                  kernel = 'gaussian')

# saveRDS(gwr_vipaic_model_bw10km, paste0(getwd(), "/data/WISL/gwr_vipaic_model_bw10km.rds"), version = 3)
gwr_vipaic_model_bw10km <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_vipaic_model_bw10km.rds")
# vipaic_model: global regression Rsqadj = 0,563 GW regression Rsq = 0,756 Rsqadj = 1













gwr_t2 <- gwr.basic(reformulate(names(si_factors_cc_2180[c(3, 16:64)]), "SI"), 
                    data = si_factors_cc_2180, 
                    bw = bandwidth * 2, 
                    dMat = dist, 
                    kernel = 'gaussian')
# ONLY BIO: global regression Rsqadj = 0,359 GW regression Rsqadj = 0,999
# BIO + first 14 ENVIREM: global regression Rsqadj = 0,368 GW regression Rsqadj = 0,931

# saveRDS(gwr_t2, paste0(getwd(), "/data/WISL/gwr_t2b.rds"), version = 3)
gwr_t2 <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_t2.rds")
gwr_t2b <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_t2b.rds")

gwr_t3 <- gwr.basic(reformulate(names(si_factors_cc_2180[c(3, 16:50)]), "SI"), 
                    data = si_factors_cc_2180, 
                    bw = bandwidth * 3, 
                    dMat = dist, 
                    kernel = 'gaussian')  # R2adj = 0.
# global regression: Rsqadj = 0,3586
# GW regression: Rsqadj = 0,6491
# saveRDS(gwr_t3, paste0(getwd(), "/data/WISL/gwr_t3.rds"), version = 3)
gwr_t3 <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_t3.rds")

gwr_t500km <- gwr.basic(reformulate(names(si_factors_cc_2180[c(3, 16:50)]), "SI"), 
                    data = si_factors_cc_2180, 
                    bw = 500000, 
                    dMat = dist, 
                    kernel = 'gaussian')  # R2adj = 0.
# global regression: Rsqadj = 0,3586
# GW regression: Rsqadj = 0,3705
# saveRDS(gwr_t500km, paste0(getwd(), "/data/WISL/gwr_t500km.rds"), version = 3)
gwr_t500km <- readRDS(file = "D:/Praca/Badania/Doktorat/data/WISL/gwr_t500km.rds")

 
# # whether this relationship is the same everywhere?
# panel.lm <- function(x,y,...) {
#         points(x, y, pch=16)
#         abline(lm(y~x))
# }
# coplot(SI ~ bio09 | dlugosc * szerokosc, data = data.frame(si_factors_gw_2180), panel = panel.lm, overlap = 0.8)
# 
# bb <- bbox(si_factors_gw_2180)
# cs <- c(10000, 10000)  # cell size 6km x 6km (for illustration)
# cc <- bb[, 1] + (cs/2)  # cell offset
# cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
# grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
# 
# grid <- SpatialGrid(grd, proj4string = CRS(as.character("+init=epsg:2180")))
# plot(grid)
# plot(si_factors_gw_2180, add = TRUE, col = adjustcolor('navyblue', alpha.f = 0.5))
# 
# DM <- gw.dist(dp.locat = coordinates(si_factors_gw_2180), rp.locat = coordinates(grid))
# gwr.res <- gwr.basic(reformulate(names(si_factors_gw_2180@data[11:60]), "SI"), data = si_factors_gw_2180, regression.points = grid, bw = 10000, dMat = DM, kernel = 'gaussian')
# 
# image(gwr.res$SDF,'bio01')
# 
# summary(lm(reformulate(names(si_factors_gw_2180@data[11:60]), "SI"), data = si_factors_gw_2180))

# library(boot)
# set.seed(4676)
# gwrcoef <- function(hpdf,i) gwr.basic(SI ~ bio01, data = data_gw_2180, regression.points = grid, bw = 10000, dMat = DM, kernel = 'gaussian')$SDF$PROF
# bootres <- boot(data_gw_2180, gwrcoef, 100)
# gwr.res$SDF$bsebio01 <- sqrt(apply(bootres$t, 2, var))
# image(gwr.res$SDF,'bsebio01')
# 
# gwr.res$SDF$biasbio01 <- bootres$t0 - apply(bootres$t, 2, mean)
# image(gwr.res$SDF,'biasbio01')

### local Moran's I ---------------------------------------------------------------------------------------------------
library(spatstat)
library(spdep)
library(maptools)
library(ape)

site_index_moran <- site_index_gps

nghbr_1 <- dnearneigh(site_index_moran, 0, 1, longlat = TRUE)
moran_1 <- localmoran(site_index_moran$SI, nb2listw(nghbr_1, zero.policy = TRUE), na.action = na.omit)

nghbr_5 <- dnearneigh(site_index_moran, 0, 5, longlat = TRUE)
moran_5 <- localmoran(site_index_moran$SI, nb2listw(nghbr_5, zero.policy = TRUE), na.action = na.omit)

site_index_moran@data <- data.frame(site_index_moran@data, as.data.frame(moran_1), as.data.frame(moran_5))

plot_map_dots(site_index_moran, "SI")
plot_map_dots(site_index_moran, "z_mean")
plot_map_dots(site_index_moran, "z_sd")
plot_map_dots(site_index_moran, "Ii")
plot_map_dots(site_index_moran, "Ii.1")