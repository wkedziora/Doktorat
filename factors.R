library(raster)
# library(rgdal)
# library(dismo)
# library(rJava)
# library(ncdf4)
library(sf)
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)

worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
worldclim <- stack(worldclim_data)

envirem_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\envirem", pattern='\\.tif$', full.names = TRUE)
envirem <- stack(envirem_data)

# gps_coord <- read_tsv("Chris/resultGPS.txt")
si <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather")) %>% dplyr::select(kraina, wiek_pan_pr, b_pion_pow_pr, tsl, okr_tsl, stan_siedl, zyzn, wilg1, wilg2, SI, szerokosc, dlugosc)
# gps <- st_as_sf(read_tsv("Chris/resultGPS.txt"), coords = c('long', 'lat'), crs = "+init=epsg:4326")
# si <- si[-10927,] #one is out of climatic dataset range
coordinates(si) <- ~ dlugosc + szerokosc
proj4string(si) <- "+init=epsg:4326" #adding WGS84 projection
# set.seed(35)
# si_sample <- si[sample(nrow(si), 10), ]

# data extraction
data <- as_tibble(data.frame(si@data$SI, si@data[,1:9], raster::extract(worldclim, si), raster::extract(envirem, si)))
names(data)[1] <- c("SI")

# correlation calculation
res_p <- cor(data, use = "complete.obs", method = c("pearson"))
# res_k <- cor(data, use = "complete.obs", method = c("kendall")) #long-run
res_s <- cor(data, use = "complete.obs", method = c("spearman"))

# plotting correlations
corrplot(res_p, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res_k, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res_s, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# singular linear regressions
varlist <- names(data)[-1]

models_lm <- lapply(varlist, function(x) {
        list(
                summary(lm(substitute(SI ~ i, list(i = as.name(x))), data = data))$coefficients, 
                summary(lm(substitute(SI ~ i, list(i = as.name(x))), data = data))$adj.r.squared)
})

# multiple linear regressions
linear_model <- lm(SI ~ ., data = data)
linear_model2 <- lm(reformulate(termlabels = names(data)[11:29], response = "SI"), data = data) #WorldClim
linear_model3 <- lm(reformulate(termlabels = names(data)[30:47], response = "SI"), data = data) #Envirem
linear_model4 <- lm(SI ~ ., data = si@data)

summary(linear_model)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)

library(MASS)
step <- stepAIC(linear_model, direction="both")
step$anova # display results
 
varlist2 <- names(data)[11:29]
models_gam <- lapply(varlist2, function(x) {
        list(
                summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$pTerms.pv, 
                summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$r.sq,
                mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data)$aic)
})

# General Additive Model
library(mgcv)
# model_gam <- gam(SI ~ s(bio_01), data = data)
model_gam2 <- gam(SI ~ s(wiek_pan_pr) + s(bio_01) + s(bio_02) + s(bio_03) + s(bio_04) + 
                          # s(bio_05) + 
                          s(bio_06) + s(bio_07) + 
                          # s(bio_08) + s(bio_09) + 
                          s(bio_10) + s(bio_11) + s(bio_12) + s(bio_13) + 
                          s(bio_14) + s(bio_15) + s(bio_16) + 
                          # s(bio_17) + 
                          s(bio_18) + s(bio_19) +
                          s(annualPET) + s(aridityIndexThornthwaite) + s(climaticMoistureIndex) + 
                          # s(continentality) + 
                          s(embergerQ) + 
                          # s(growingDegDays0) + 
                          s(growingDegDays5) + s(maxTempColdest) + s(minTempWarmest) + 
                          # s(monthCountByTemp10) + 
                          s(PETColdestQuarter) + s(PETDriestQuarter) + s(PETseasonality) + s(PETWarmestQuarter) + 
                          # s(PETWettestQuarter) + 
                          s(thermicityIndex) + s(topoWet) + s(tri), 
                  data = data)
summary(model_gam2)

model_gam3 <- gam(SI ~ s(bio_01) + s(bio_02) + s(bio_03) + s(bio_04) + 
                          # s(bio_05) + 
                          s(bio_06) + s(bio_07) + 
                          # s(bio_08) + s(bio_09) + 
                          s(bio_10) + s(bio_11) + s(bio_12) + s(bio_13) + 
                          s(bio_14) + s(bio_15) + s(bio_16) + 
                          # s(bio_17) + 
                          s(bio_18) + s(bio_19) +
                          s(annualPET) + s(aridityIndexThornthwaite) + s(climaticMoistureIndex) + 
                          # s(continentality) + 
                          s(embergerQ) + 
                          # s(growingDegDays0) + 
                          s(growingDegDays5) + s(maxTempColdest) + s(minTempWarmest) + 
                          # s(monthCountByTemp10) + 
                          s(PETColdestQuarter) + s(PETDriestQuarter) + s(PETseasonality) + s(PETWarmestQuarter) + 
                          # s(PETWettestQuarter) + 
                          s(thermicityIndex) + s(topoWet) + s(tri), 
                  data = data)
summary(model_gam3)

