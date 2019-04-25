# Wczytanie bibliotek
library(tidyverse)
library(ggplot2)   # Wizualizacje
library(earth)     # dopasowanie modelu MARS
library(caret)     # automatyzacja tuningu parametrów
library(vip)       # ważność zmiennych
library(pdp)       # partial dependence plots
library(parallel)  # obliczenia równoległe
library(doParallel)# obliczenia równoległe
library(feather)

theme_set(
        theme_bw() +
                theme(legend.position = "top", 
                      axis.text = element_text(size = 11), 
                      axis.title = element_text(size = 12, face = "bold"))
)

si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather")) %>% droplevels()

# names(si_factors)

si_factors_cc <- si_factors[complete.cases(si_factors$bio01) & complete.cases(si_factors$tri), ] %>% filter(SDI < 10000)
# summary(si_factors_cc)

set.seed(42)
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(si_factors_cc$SI, p = 0.80, list = FALSE)
# select 20% of the data for validation
validation <- si_factors_cc[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- si_factors_cc[validation_index,]

# formula from VIF and stepwise regression (models.R):
model_formula <- as.formula(SI ~ wiek_pan_pr + b_pion_pow_pr + zyzn + wilg1 + 
                                    wlasc + n_drzew + n_gat + SDI + jez_dist + drzek_dist + rzeki_dist + 
                                    dlugosc + bio21 + bio24 + bio25 + bio27 + bio35 + aridityIndexThornthwaite + 
                                    growingDegDays0 + maxTempColdest + minTempWarmest + monthCountByTemp10 + 
                                    PETDriestQuarter + PETWettestQuarter + tri)

## 10-fold CV ---------------------------------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, ## 10-fold
                           repeats = 5, ## repeated five times
                           verboseIter = T, ## printing a training log
                           allowParallel = T) ## use parallel processing

# multiple regression --------------------------------------------------------------------------------------------
set.seed(42)
mlr_model <- train(model_formula,
                   data = dataset,
                   method = "lm", #"glmStepAIC", 
                   metric = "RMSE",
                   trControl = fitControl #trainControl(method = "cv", number = 10),
                   # preProcess = c("zv", "center", "scale")
)
# --- with LOOCV no further improvment ---
# saveRDS(mlr_model, file = "D:/Praca/Badania/Doktorat/model/mlr_model.rds")
# mlr_model <- readRDS(file = "D:/Praca/Badania/Doktorat/model/mlr_model.rds")

# Model accuracy
mlr_model$results
# Final model coefficients
mlr_model$finalModel
# Summary of the model
summary(mlr_model$finalModel)

mlr_predict <- predict(mlr_model, validation)
postResample(pred = mlr_predict, obs = validation$SI)
mlr_predict_df <- data.frame(pred = mlr_predict, SI = validation$SI)
ggplot(data = mlr_predict_df, aes(x = SI, y = (pred - SI))) +
        geom_point()

plot(varImp(mlr_model), 20, xlab = "wpływ zmiennej")
# xxx <- as.data.frame(varImp(mlr_model)[1])
# ggplot(data = xxx, aes(x = reorder(rownames(xxx), Overall, FUN = max), y = Overall)) +
#         geom_point() + coord_flip() +
#         labs(x = "nazwa zmiennej", y = "wpływ zmiennej") +
#         scale_x_discrete(labels = c("Lasy Państwowe", "parki narodowe", "lasy prywatne", "inne Skarbu Państwa"))

imp_mlr <- varImp(mlr_model) 
imp_mlr <- imp_mlr$importance %>%
        as.data.frame() %>%
        mutate(variable = row.names(.)) %>%
        filter(Overall > 5) %>% 
        arrange(-Overall)

par(mfrow = c(5, 4), mar = c(2, 2, 2, 1))

plotmo(mlr_model$finalModel, all1 = T, degree1 = imp_mlr$variable, clip = F, caption = "", pt.col = 0, 
       trace = 1, do.par = F, main = imp_mlr$variable)
dev.off()

plotres(mlr_model$finalModel, which = 3, main = "", xlab = "model", ylab = "wartości rezydualne")

# library(visreg)
# visreg(mlr_model$finalModel, c("bio21", "bio27"))

# # random forest ----------------------------------------------------------------------------------
set.seed(42)
tunegrid <- expand.grid(.mtry = 19)#c(1:30))

test <- c("wiek_pan_pr", "b_pion_pow_pr", "zyzn", "wilg1", "wlasc", "n_drzew", "n_gat", "SDI", "jez_dist", "drzek_dist", "rzeki_dist", 
          "dlugosc", "bio21", "bio24", "bio25", "bio27", "bio35", "aridityIndexThornthwaite", 
          "growingDegDays0", "maxTempColdest", "minTempWarmest", "monthCountByTemp10", "PETDriestQuarter", "PETWettestQuarter", "tri")

rf_model <- train(x = dataset[test], 
                  y = dataset[["SI"]], 
                  method = 'rf', 
                  trControl = fitControl, #trainControl("oob", allowParallel = T), #trainControl(method = "cv", number = 10),
                  ntree = 1000, 
                  metric = 'RMSE',
                  tuneGrid = tunegrid,
                  preProcess = c("zv", "center", "scale"),
                  importance = TRUE,
                  keep.inbag = TRUE,
                  keep.forest = TRUE)

# saveRDS(rf_model, file = "D:/Praca/Badania/Doktorat/model/rf_model.rds")
# rf_model <- readRDS(file = "D:/Praca/Badania/Doktorat/model/rf_model.rds")

# Model accuracy
rf_model$results
# Final model coefficients
rf_model$finalModel
# Summary of the model
summary(rf_model$finalModel)

plot(rf_model)
plot(rf_model$finalModel)

varImpPlot(rf_model$finalModel)

plotmo(rf_model$finalModel)

plot(varImp(rf_model$finalModel), 15, xlab = "wpływ zmiennej")

rf_predict <- predict(rf_model, validation)
postResample(pred = rf_predict, obs = validation$SI)

imp_rf <- varImp(rf_model) 
imp_rf <- imp_rf$importance %>%
        as.data.frame() %>%
        mutate(variable = row.names(.)) %>%
        filter(Overall > 0)

par(mfrow = c(8, 4), mar = c(2, 1, 2, 1))

plotmo(rf_model, all1 = T, degree1 = imp_rf$variable, clip = F, caption="", pt.col = 0, 
       trace = 1, do.par = F, main = imp_rf$variable)

plotres(rf_model$finalModel, which = 3)
plotres(rf_model$finalModel, which = 4)

library(randomForest)
dataset2 <- as.data.frame(dataset)
final_model <- randomForest(x = dataset2[test], 
                            y = dataset2[["SI"]],   
                            ntree = 500, 
                            mtry = 19,
                            importance = TRUE,
                            keep.inbag = TRUE,
                            keep.forest = TRUE)

# saveRDS(final_model, file = "D:/Praca/Badania/Doktorat/model/final_model.rds")
# final_model <- readRDS(file = "D:/Praca/Badania/Doktorat/model/final_model.rds")

plot(final_model)
varImpPlot(final_model)
plotmo(final_model)

plot(varImp(final_model), 15, xlab = "wpływ zmiennej")

imp_rf <- varImp(final_model) 
imp_rf <- imp_rf %>%
        as.data.frame() %>%
        mutate(variable = row.names(.)) %>%
        filter(Overall > 0)

par(mfrow = c(8, 4), mar = c(2, 1, 2, 1))

plotmo(final_model, all1 = T, degree1 = imp_rf$variable, clip = F, caption="", pt.col = 0, 
       trace = 1, do.par = F)#, main = imp_rf$variable)

library(randomForestExplainer)
# explain_forest(final_model, interactions = TRUE, data = dataset)

# min_depth_frame <- min_depth_distribution(rf_model$finalModel)
# saveRDS(min_depth_frame, file = "D:/Praca/Badania/Doktorat/rf_model/min_depth_frame.rds")
min_depth_frame <- readRDS(file = "D:/Praca/Badania/Doktorat/rf_model/min_depth_frame.rds")
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

# importance_frame <- measure_importance(rf_model$finalModel)
# saveRDS(importance_frame, file = "D:/Praca/Badania/Doktorat/rf_model/importance_frame.rda")
importance_frame <- readRDS("D:/Praca/Badania/Doktorat/rf_model/importance_frame.rda")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)

(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
# interactions_frame <- min_depth_interactions(rf_model$finalModel, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
# saveRDS(interactions_frame, file = "D:/Praca/Badania/Doktorat/rf_model/interactions_frame.rds")
interactions_frame <- readRDS(file = "D:/Praca/Badania/Doktorat/rf_model/interactions_frame.rds")

plot_min_depth_interactions(interactions_frame)

plot_predict_interaction(rf_model$finalModel, dataset, "wiek_pan_pr", "bio27")

# MARS1 /EARTH --------------------------------------------------------------------------------------------------------
hyper_grid1 <- expand.grid(degree = 1, # jaki stopień interakcji 
                           nprune = 34)#floor(seq(2, 100, length.out = 10))) # ile wyrazów ma pozostać w modelu
# ctrl <- trainControl(method = "cv", number = 10,
#                      verboseIter = T, allowParallel = T)

set.seed(42)
mars_model1 <- train(model_formula,
                     data = dataset,
                     method = "earth",
                     metric = "RMSE",
                     trControl = fitControl,
                     tuneGrid = hyper_grid1,
                     keepxy = TRUE
)

plot(mars_model1$finalModel)
summary(mars_model1)
summary(mars_model1$finalModel)

mars_predict1 <- predict(mars_model1, validation)
postResample(pred = mars_predict1, obs = validation$SI)

# saveRDS(mars_model1, file = "D:/Praca/Badania/Doktorat/model/mars_model1.rds")
# mars_model1 <- readRDS(file = "D:/Praca/Badania/Doktorat/model/mars_model1.rds")

evimp(mars_model1$finalModel, trim = TRUE)
plot(varImp(mars_model1), 15, xlab = "wpływ zmiennej")

partial(mars_model1, pred.var = c("tri", "PETDriestQuarter"), grid.resolution = 10) %>% autoplot()
partial(mars_model1, pred.var = c("PETDriestQuarter", "tri"), grid.resolution = 10) %>% 
        plotPartial(levelplot = FALSE, zlab = "SI", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))

imp_mars1 <-  varImp(mars_model1) 
imp_mars1 <- imp_mars1$importance %>% 
        as.data.frame() %>%
        mutate(variable = row.names(.)) %>%
        filter(Overall > 0)

par(mfrow = c(5, 4), mar = c(2, 1, 2, 1))

plotmo(mars_model1$finalModel, all1 = T, degree1 = imp_mars1$variable, clip = F, caption="", pt.col = 0, 
       trace = 1, do.par = F, main = imp_mars1$variable)
# MARS2 /EARTH --------------------------------------------------------------------------------------------------------

hyper_grid2 <- expand.grid(degree = 1:2, # jaki stopień interakcji 
                           nprune = 45)#floor(seq(2, 100, length.out = 10))) # ile wyrazów ma pozostać w modelu
# ctrl <- trainControl(method = "cv", number = 10,
#                      verboseIter = T, allowParallel = T)

set.seed(42)
mars_model2 <- train(model_formula,
                     data = dataset,
                     method = "earth",
                     metric = "RMSE",
                     trControl = fitControl,
                     tuneGrid = hyper_grid2
)

plot(mars_model2)
summary(mars_model2)
summary(mars_model2$finalModel)

mars_predict2 <- predict(mars_model2, validation)
postResample(pred = mars_predict2, obs = validation$SI)

# saveRDS(mars_model2, file = "D:/Praca/Badania/Doktorat/model/mars_model2.rds")
# mars_model2 <- readRDS(file = "D:/Praca/Badania/Doktorat/model/mars_model2.rds")

plot.evimp(evimp(mars_model2$finalModel, trim = TRUE))

plot(varImp(mars_model2), 40, xlab = "wpływ zmiennej")

partial(mars_model2, pred.var = "n_drzew", grid.resolution = 10) %>% autoplot()
partial(mars_model2, pred.var = "SDI", grid.resolution = 10) %>% autoplot()
partial(mars_model2, pred.var = c("n_drzew", "SDI"), grid.resolution = 10) %>% 
        plotPartial(levelplot = FALSE, zlab = "SI", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))

imp_mars2 <-  varImp(mars_model2) 
imp_mars2  <-  imp_mars2$importance %>%
        as.data.frame() %>%
        mutate(variable = row.names(.)) %>%
        filter(Overall > 0)

par(mfrow = c(5, 4), mar = c(2, 1, 2, 1))

plotmo(mars_model2$finalModel, all1 = T, degree1 = imp_mars2$variable, clip = F, caption="", pt.col = 0, 
       trace = 1, do.par = F, main = imp_mars1$variable)
# MARS3 /EARTH --------------------------------------------------------------------------------------------------------
# 
# hyper_grid <- expand.grid(degree = 1:3, # jaki stopień interakcji 
#                           nprune = floor(seq(2, 100, length.out = 10))) # ile wyrazów ma pozostać w modelu
# # ctrl <- trainControl(method = "cv", number = 10,
# #                      verboseIter = T, allowParallel = T)
# 
# set.seed(42)
# mars_model3 <- train(model_formula,
#                      data = dataset,
#                      method = "earth",
#                      metric = "RMSE",
#                      trControl = fitControl,
#                      tuneGrid = hyper_grid
# )
# 
# plot(mars_model3)
# summary(mars_model3)
# 
# mars_model3$bestTune
# summary(mars_model3$finalModel)
# 
# plot(mars_model3, which = 1)

# saveRDS(mars_model3, file = "D:/Praca/Badania/Doktorat/model/mars_model3.rds")
# mars_model3 <- readRDS(file = "D:/Praca/Badania/Doktorat/model/mars_model3.rds")

# Ważność parametrów
plot(varImp(mars_model3), 15, xlab = "wpływ zmiennej")

vip(mars_model3, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
vip(mars_model3, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

# Efekty cząstkowe (partial )
partial(mars_model3, pred.var = "wiek_pan_pr", grid.resolution = 10) %>% autoplot()
partial(mars_model3, pred.var = "bio27", grid.resolution = 10) %>% autoplot()
partial(mars_model3, pred.var = c("wiek_pan_pr", "bio27"), grid.resolution = 10) %>% 
        plotPartial(levelplot = FALSE, zlab = "SI", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))

mars_predict3 <- predict(mars_model3, validation)
postResample(pred = mars_predict3, obs = validation$SI)
# t.test(mars_predict - validation$SI)
# cor.test(mars_predict, validation$SI)
mars_predict3_df <- data.frame(pred = mars_predict3[, 1], SI = validation$SI)
ggplot(data = mars_predict3_df) +
        geom_point(aes(x = SI, y = (pred - SI))) + 
        geom_hline(yintercept = 0) +
        labs(x = "SI", y = "resid")


# model comparison ----------------------------------------------------------------------------------------------------

results <- resamples(list(MLR = mlr_model, 
                          # PCR = pca_model, 
                          # PLS = pls_model,
                          # GLM_NET = rr_model,
                          # GAM = gam_model,
                          RFR = rf_model,
                          MARS1 = mars_model1,
                          MARS2 = mars_model2
                          # MARS3 = mars_model3
                          # Bagged_MARS = Bmars_model
))

summary(results)

compare_models(mars_model2, mars_model1)
compare_models(mars_model2, rf_model)

# Draw box plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

stopCluster(cl)
registerDoSEQ()

# # principal component regression ----------------------------------------------------------------------------------
# set.seed(42)
# pca_model <- train(model_formula, 
#                    data = dataset, 
#                    method = "pcr",
#                    trControl = fitControl, #trainControl(method = "cv", number = 10),
#                    metric = "RMSE",
#                    preProcess = c("zv", "center", "scale"),
#                    tuneLength = 20
# )
# 
# # Model accuracy
# pca_model$results
# # Final model coefficients
# pca_model$finalModel
# # Summary of the model
# summary(pca_model$finalModel)
# 
# plot(pca_model)
# 
# plot(varImp(pca_model), 10, main = "Regresja liniowa wielokrotna", xlab = "wpływ zmiennej [")
# 
# 
# pca_predict <- predict(pca_model, validation)
# postResample(pred = pca_predict, obs = validation$SI)

# # partial least squares regression --------------------------------------------------------------------------------
# set.seed(42)
# pls_model <- train(model_formula,
#                    data = dataset,
#                    method = "pls",
#                    trControl = fitControl, #trainControl(method = "cv", number = 10),
#                    metric = "RMSE",
#                    preProcess = c("zv", "center", "scale"),
#                    tuneLength = 20
# )
# 
# plot(pls_model)
# 
# pls_predict <- predict(pls_model, validation)
# postResample(pred = pls_predict, obs = validation$SI)

# # regularized regression -------------------------------------------------------------------------------------------
# set.seed(42)
# rr_model <- train(model_formula, 
#                   data = dataset, 
#                   method = "glmnet",
#                   trControl = fitControl, #trainControl(method = "cv", number = 10),
#                   metric = "RMSE",
#                   preProcess = c("zv", "center", "scale"),
#                   tuneLength = 10
# )
# 
# plot(rr_model)
# 
# rr_predict <- predict(rr_model, validation)
# postResample(pred = rr_predict, obs = validation$SI)

# Generalized Additive Model  ---------------------------------------------------------------------------------------
# set.seed(42)
# gam_model <- train(model_formula, 
#                    data = dataset, 
#                    method = "gam",
#                    trControl = fitControl, #trainControl(method = "cv", number = 10),
#                    metric = "RMSE",
#                    preProcess = c("zv", "center", "scale"),
#                    tuneLength = 10
# )
# 
# gam_model$converged
# plot(gam_model)
# summary(gam_model)
# 
# gam_predict <- predict(gam_model, validation)
# postResample(pred = gam_predict, obs = validation$SI)

# # # bagged MARS/EARTH --------------------------------------------------------------------------------------------------
# set.seed(42)
# Bmars_model <- train(model_formula,
#                      data = dataset,
#                      method = "bagEarth",
#                      trControl = fitControl, #trainControl(method = "cv", number = 10),
#                      metric = "RMSE"
# )
# 
# Bmars_predict <- predict(Bmars_model, validation)
# postResample(pred = Bmars_predict, obs = validation$SI)
# # --- results not better than ordinary MARS ---