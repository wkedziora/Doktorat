library(plyr)
library(caret)
library(raster)
library(sf)
library(tidyverse)
library(corrplot)
library(feather)

# #WorldClim data is used in CliMond set so I am rejecting it from analysis, however, they are in finer resolution
# # worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
# # worldclim <- stack(worldclim_data)
# 
# climond_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\CliMond", pattern='\\.tif$', full.names = TRUE)
# climond <- stack(climond_data[1:35])
# 
# envirem_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\envirem", pattern='\\.tif$', full.names = TRUE)
# envirem <- stack(envirem_data)
# 
# # distances and angles to closests (1) lakes and water bodies, (2) big rivers, (3) medium rivers
# water_dist <- read_csv2("shp/distance/distance.txt") %>% dplyr::select(-c("FID", "SZEROKOSC", "DLUGOSC", "ROK_W_CYKL"))
# 
# si <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather")) %>%
#         left_join(., water_dist, by = c("nr_punktu" = "NR_PUNKTU")) %>%
#         dplyr::select(SI, kraina, wiek_pan_pr, b_pion_pow_pr, zyzn, wilg1, szerokosc, dlugosc, wlasc,
#                       n_drzew, n_gat, SDI, jez_dist, drzek_dist, rzeki_dist) # tsl omitted due to multicolinearity
# 
# # si <- si[-10927,] #one is out of climatic dataset range
# coordinates(si) <- ~ dlugosc + szerokosc
# proj4string(si) <- "+init=epsg:4326" #adding WGS84 projection
# 
# # data extraction
# si_factors <- as_tibble(data.frame(si@data, coordinates(si), raster::extract(climond, si), raster::extract(envirem, si)))
# summary(si_factors) # NA's are out of raster bounds

# write_feather(si_factors, paste0(getwd(), "/data/WISL/si_factors.feather"))

###################################################################
## Wczytanie danych
si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather"))
si_factors_cc <- si_factors[complete.cases(si_factors$bio01) & complete.cases(si_factors$tri), ]# c(1:4, 8:64)]
# si_factors_cc <- si_factors[complete.cases(si_factors$bio01) & complete.cases(si_factors$tri), ]

# correlation calculation
res_p <- cor(si_factors_cc %>% dplyr::mutate_if(sapply(si_factors_cc, is.factor), as.numeric), use = "complete.obs", method = c("pearson"))
# res_k <- cor(data, use = "complete.obs", method = c("kendall")) #long-running
res_s <- cor(si_factors_cc %>% dplyr::mutate_if(sapply(si_factors_cc, is.factor), as.numeric), use = "complete.obs", method = c("spearman"))

# plotting correlations
corrplot(res_p, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# corrplot(res_k, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res_s, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# multicolinearity testing
library(mctest)
data_cor <- as.data.frame(si_factors_cc %>% dplyr::mutate_if(sapply(si_factors_cc, is.factor), as.numeric) %>% dplyr::select(-SI))
options("scipen" = 100, "digits" = 4)
omcdiag(data_cor, si_factors_cc$SI) # CHI^2 highly significant implying the presence of multicollinearity in the model specification
imcdiag(data_cor, si_factors_cc$SI) #details about multicolinearity, high VIF implicts correlations between variables

# basic linear regressions
linear_model <- lm(SI ~ ., data = si_factors_cc)
summary(linear_model)
plot(linear_model)

# multiple linear regressions ---------------------------------------------------------------------------------
set.seed(42)
training.samples <- si_factors_cc$SI %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- si_factors_cc[training.samples, ]
test.data <- si_factors_cc[-training.samples, ]

# model1 <- lm(SI ~ . , data = train.data) #problem with perfect multicolienarity of bio05 and bio06 thus:
model1 <- lm(SI ~ . -bio06, data = train.data)
summary(model1)
# model2 <- lm(SI ~ wiek_pan_pr + monthCountByTemp10 + PETDriestQuarter + topoWet + tri + bio01, data = train.data)
alias(model1) # checking if there is perfcet multicolinearity - if so we cannot move on

# Make predictions
predictions <- model1 %>% predict(test.data)
# Model performance
data.frame(
        RMSE = RMSE(predictions, test.data$SI),
        R2 = R2(predictions, test.data$SI)
)

car::vif(model1)

# removing multicolinearity
model_vif <- model1
# Choose a VIF cutoff under which a variable is retained (Zuur et al. 2010 MEE recommends 2)
cutoff <- 10
# Create function to sequentially drop the variable with the largest VIF until all variables have VIF > cutoff
flag <- TRUE
viftable <- data.frame()
while(flag == TRUE) {
        vfit = car::vif(model_vif)
        viftable = rbind.fill(viftable, as.data.frame(t(vfit[,1])))
        if(max(vfit) > cutoff) { model_vif = update(model_vif, as.formula(paste(".", "~", ".", "-", names(which.max(vfit[,1]))))) }
        else { flag = FALSE } 
}
# Look at the final model
print(model_vif)
# And associated VIFs
print(vfit)
# And show the order in which variables were dropped
print(viftable)

colnames(viftable)[apply(viftable, 1, which.max)]

for (i in seq_along(viftable)){
        max = as.numeric(which.max(viftable[i,]))
        t = viftable[i, max]
        print(t)
}

summary(model_vif)

# Make predictions
predictions_vif <- model_vif %>% predict(test.data)
# Model performance
data.frame(
        RMSE = RMSE(predictions_vif, test.data$SI),
        R2 = R2(predictions_vif, test.data$SI)
)

library(MASS)
model_step <- stepAIC(model_vif, scope = list(upper = ~., lower = ~1), direction = "both")
# model1_step <- stepAIC(model1, scope = list(upper = ~., lower = ~1), direction = "both")
# car::vif(model1_step)
model_step$anova # display results
summary(model_step)

car::vif(model_step)

# Make predictions
predictions_step <- model_step %>% predict(test.data)
# Model performance
data.frame(
        RMSE = RMSE(predictions_step, test.data$SI),
        R2 = R2(predictions_step, test.data$SI)
)

# General Additive Model --------------------------------------------------------------------------------------
library(mgcv)
# varlist <- names(si_factors_cc)[c(11:54,56:63)] #only integers, no classes
# varlist2 <- varlist[-1]
# models_gam <- lapply(varlist2, function(x) {
#         list(
#                 summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$pTerms.pv, 
#                 summary(mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data))$r.sq,
#                 mgcv::gam(substitute(SI ~ s(i), list(i = as.name(x))), data = data)$aic)
# })

model_gam <- gam(SI ~ s(bio22), data = si_factors_cc)
model_gam$converged
summary(model_gam)
plot(model_gam)

model_gam2 <- gam(SI ~ s(wiek_pan_pr) + b_pion_pow_pr + zyzn + wilg1 + wlasc + s(SDI) +
                          s(bio01) + 
                          # s(bio02) + 
                          s(bio03) + s(bio04) + 
                          # s(bio05) +
                          s(bio06) + s(bio07) + 
                          # bio08 + 
                          s(bio09) + 
                          # bio10 + bio11 + bio12 + 
                          # s(bio13) + 
                          s(bio14) + s(bio15) + 
                          # bio16 +
                          s(bio17) + 
                          # s(bio18) + s(bio19) +
                          s(bio20) + s(bio21) + 
                          # s(bio22) +
                          s(bio23) + 
                          # s(bio24) + 
                          s(bio25) + s(bio26) + s(bio27) + s(bio28) + 
                          # s(bio29) + 
                          # bio30 + 
                          s(bio31) + 
                          # s(bio32) + 
                          # bio33 + 
                          # bio34 +
                          # s(bio35) +
                          s(annualPET) + s(aridityIndexThornthwaite) + 
                          # s(climaticMoistureIndex) + 
                          s(continentality) +
                          # s(embergerQ) +
                          # growingDegDays0 + #turned-off due to lack of importance -> p > 0.05
                          # growingDegDays5 +
                          s(maxTempColdest) +
                          s(minTempWarmest) + 
                          # monthCountByTemp10 +
                          s(PETColdestQuarter) +
                          # PETDriestQuarter +
                          s(PETseasonality) + 
                          # s(PETWarmestQuarter) +
                          # PETWettestQuarter +
                          s(thermicityIndex) + s(topoWet) + s(tri), 
                  data = si_factors_cc)
model_gam2$converged
summary(model_gam2)
plot(model_gam2)

# library(gamsel)
# gamsel(SI ~ s(bio01), data = si_factors_cc)

#removing age lowers the R^2 (by 28%) to 20%
model_gam_wiek <- gam(SI ~ s(wiek_pan_pr), data = si_factors_cc)
model_gam_wiek$converged
summary(model_gam_wiek)
plot(model_gam_wiek)
# 
# ### geologia - zbyt ogólne dane
# litography_500k <- read_feather(paste0(getwd(), "/data/WMS/litography_500k.feather"))
# litography_50k <- read_feather(paste0(getwd(), "/data/WMS/litography_50k.feather"))
# BDL <- read_feather(paste0(getwd(), "/data/BDL/BDL.feather")) # BDL data have only 30% of points in sites
# 
# summary(lm(SI ~ soil_spe_1, data = BDL))
# summary(lm(SI ~ geol_dep_1, data = BDL))
# summary(lm(SI ~ arable_f_1, data = BDL))
# summary(lm(SI ~ degradat_1, data = BDL))
# summary(lm(SI ~ soil_speci, data = BDL))
# summary(lm(SI ~ geol_depos, data = BDL))
# summary(lm(SI ~ tsl, data = BDL))
# summary(lm(SI ~ wiek_pan_pr, data = BDL))
# 
