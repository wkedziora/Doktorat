library(raster)
# library(rgdal)
# library(dismo)
# library(rJava)
# library(ncdf4)
library(sf)
library(tidyverse)
library(corrplot)
library(feather)
# library(PerformanceAnalytics)

#WorldClim data is used in CliMond set so I am rejecting it from analysis, however, they are in finer resolution
# worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
# worldclim <- stack(worldclim_data)

climond_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\CliMond", pattern='\\.tif$', full.names = TRUE)
climond <- stack(climond_data[1:35])

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
data <- as_tibble(data.frame(si@data$SI, si@data[,1:9], raster::extract(climond, si), raster::extract(envirem, si)))
names(data)[1] <- c("SI")

# correlation calculation
res_p <- cor(data[-c(2:10)], use = "complete.obs", method = c("pearson"))
# res_k <- cor(data, use = "complete.obs", method = c("kendall")) #long-run
res_s <- cor(data[-c(2:10)], use = "complete.obs", method = c("spearman"))

# plotting correlations
corrplot(res_p, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# corrplot(res_k, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res_s, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# singular linear regressions
varlist <- names(data)[-1]

models_lm <- lapply(varlist, function(x) {
        list(
                # summary(lm(substitute(SI ~ i, list(i = as.name(x))), data = data))$coefficients, 
                summary(lm(substitute(SI ~ i, list(i = as.name(x))), data = data))$adj.r.squared)
})

linear_model <- lm(SI ~ ., data = data)

summary(linear_model)

# multiple linear regressions
linear_model <- lm(SI ~ ., data = data)
linear_model2 <- lm(reformulate(termlabels = names(data)[11:29], response = "SI"), data = data) #CliMond/WorldClim
linear_model3 <- lm(reformulate(termlabels = names(data)[30:47], response = "SI"), data = data) #Envirem
linear_model4 <- lm(SI ~ ., data = si@data)

summary(linear_model)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)

library(MASS)
step <- stepAIC(linear_model, scope = list(upper = ~., lower = ~1), direction = "both")
step$anova # display results
summary(step)
 
varlist <- names(data)[c(11:54,56:63)] #only integers, no classes
varlist2 <- varlist[-1]
models_gam <- lapply(varlist2, function(x) {
        list(
                summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$pTerms.pv, 
                summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$r.sq,
                mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data)$aic)
})

# General Additive Model
library(mgcv)
# model_gam <- gam(SI ~ s(bio01), data = data)
model_gam2 <- gam(SI ~ s(wiek_pan_pr) + s(bio01) + s(bio02) + s(bio03) + s(bio04) + 
                          # s(bio05) + 
                          s(bio06) + s(bio07) + 
                          # s(bio08) + s(bio09) + 
                          s(bio10) + s(bio11) + s(bio12) + s(bio13) + 
                          s(bio14) + s(bio15) + s(bio16) + 
                          # s(bio17) + 
                          s(bio18) + s(bio19) + s(bio20) + s(bio21) + s(bio22) + s(bio23) + 
                          s(bio24) + s(bio25) + s(bio26) + s(bio27) + s(bio28) + s(bio29) + 
                          s(bio30) + s(bio31) + s(bio32) + s(bio33) + s(bio34) + s(bio35) +
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

#removing age lowers the R^2 (by 28%) to 20%
model_gam3 <- gam(SI ~ s(bio01) + s(bio02) + s(bio03) + s(bio04) + 
                          # s(bio05) + 
                          s(bio06) + s(bio07) + 
                          # s(bio08) + s(bio09) + 
                          s(bio10) + s(bio11) + s(bio12) + s(bio13) + 
                          s(bio14) + s(bio15) + s(bio16) + 
                          # s(bio17) + 
                          s(bio18) + s(bio19) +
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

### geologia
litography_500k <- read_feather(paste0(getwd(), "/data/WMS/litography_500k.feather"))
litography_50k <- read_feather(paste0(getwd(), "/data/WMS/litography_50k.feather"))
BDL <- read_feather(paste0(getwd(), "/data/BDL/BDL.feather")) # BDL data have only 30% of points in sites

summary(lm(SI ~ soil_spe_1, data = BDL))
summary(lm(SI ~ geol_dep_1, data = BDL))
summary(lm(SI ~ arable_f_1, data = BDL))
summary(lm(SI ~ degradat_1, data = BDL))
summary(lm(SI ~ soil_speci, data = BDL))
summary(lm(SI ~ geol_depos, data = BDL))
summary(lm(SI ~ tsl, data = BDL))
summary(lm(SI ~ wiek_pan_pr, data = BDL))

