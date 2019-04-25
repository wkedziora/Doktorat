###########################################################
#             Warsztaty Machine Learning w R              #
#                       LabMasters                        #
#                    Piotr Ćwiakowski                     #
#                    Model MARS/EARTH                     # 
###########################################################

# Wczytanie bibliotek
library(ggplot2)   # Wizualizacje
library(earth)     # dopasowanie modelu MARS
library(caret)     # automatyzacja tuningu parametrów
library(vip)       # ważność zmiennych
library(pdp)       # partial dependence plots
library(parallel)  # obliczenia równoległe
library(doParallel)# obliczenia równoległe
library(feather)

# Wczytanie danych
# f <- read.csv('0 Dane/footballers2.csv', sep = ';')
# rownames(f) <- f$name
# f$name <- NULL
si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather"))

names(si_factors)

# si_factors_cc <- si_factors[complete.cases(si_factors$bio04) & complete.cases(si_factors$tri), c(1, 3, 13:65)]
si_factors_cc <- si_factors[complete.cases(si_factors$bio04) & complete.cases(si_factors$tri), c(1:4, 8:9, 13:65)]
summary(si_factors_cc)

# Szacowanie modelu
SI_1 <- earth(SI ~ ., data = si_factors_cc)
summary(SI_1)
plot(SI_1, which = 1)

# Pozwalamy na interakcje:
SI_2 <- earth(SI ~ ., data = si_factors_cc, degree = 2)
summary(SI_2)
plot(SI_2, which = 1)

# Tuning
hyper_grid <- expand.grid(degree = 1:3, # jaki stopień interakcji 
                          nprune = floor(seq(2, 100, length.out = 10))) # ile wyrazów ma pozostać w modelu
ctrl <- trainControl(method = "cv", number = 10,
                     verboseIter = T, allowParallel = T)

# formula from VIF and stepwise regression:
model_formula <- as.formula(SI ~ wiek_pan_pr + b_pion_pow_pr + zyzn + wilg1 + 
                                 bio13 + bio21 + bio24 + bio27 + bio35 + growingDegDays0 + 
                                 maxTempColdest + minTempWarmest + monthCountByTemp10 + PETDriestQuarter + 
                                 PETseasonality + tri)
# Walidacja krzyżowa
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)
SI_Final <- train(model_formula,
                  data = si_factors_cc,
                  method = "earth",
                  metric = "RMSE",
                  trControl = ctrl,
                  tuneGrid = hyper_grid
)
SI_Final$bestTune
SI_Final
ggplot(SI_Final)

stopCluster(cl)
registerDoSEQ()

# Wydruk modelu finalnego
summary(SI_Final$finalModel)
plotd(SI_Final$finalModel)
plot(SI_Final, which = 1)

# Ważność parametrów
vip(SI_Final, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
vip(SI_Final, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

# Efekty cząstkowe (partial )
partial(SI_Final, pred.var = "wiek_pan_pr", grid.resolution = 10) %>% autoplot()
partial(SI_Final, pred.var = "bio27", grid.resolution = 10) %>% autoplot()
partial(SI_Final, pred.var = c("wiek_pan_pr", "bio27"), grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))

# Ćwiczenie
# Przeszukaj dokładniej siatkę hiperparametrów wokół punktu optymalnego.

# Ćwiczenie sprawdź jak zachowują się inne modele - regresja liniowa, regresje
# z regularyzacją.

lm <- train(model_formula,
            data = si_factors_cc,
            method = "lm",
            metric = "RMSE",
            trControl = ctrl
)
lm
summary(lm)

# Analiza wyników
summary(resamples(list(MARS = SI_Final, LM = lm)))

# Więcej informacji:
# http://uc-r.github.io/mars

# multiple regression
set.seed(123)
cv_model1 <- train(
        model_formula, 
        data = si_factors_cc, 
        method = "lm",
        metric = "RMSE",
        trControl = trainControl(method = "cv", number = 10),
        preProcess = c("zv", "center", "scale")
)

# principal component regression
set.seed(123)
cv_model2 <- train(
        model_formula, 
        data = si_factors_cc, 
        method = "pcr",
        trControl = trainControl(method = "cv", number = 10),
        metric = "RMSE",
        preProcess = c("zv", "center", "scale"),
        tuneLength = 20
)

# partial least squares regression
set.seed(123)
cv_model3 <- train(
        model_formula, 
        data = si_factors_cc, 
        method = "pls",
        trControl = trainControl(method = "cv", number = 10),
        metric = "RMSE",
        preProcess = c("zv", "center", "scale"),
        tuneLength = 20
)

# regularized regression
set.seed(123)
cv_model4 <- train(
        model_formula, 
        data = si_factors_cc, 
        method = "glmnet",
        trControl = trainControl(method = "cv", number = 10),
        metric = "RMSE",
        preProcess = c("zv", "center", "scale"),
        tuneLength = 10
)

# GAM
set.seed(123)
cv_model5 <- train(
        model_formula, 
        data = si_factors_cc, 
        method = "gam",
        trControl = trainControl(method = "cv", number = 10),
        metric = "RMSE",
        preProcess = c("zv", "center", "scale"),
        tuneLength = 10
)

# extract out of sample performance measures
summary(resamples(list(
        Multiple_regression = cv_model1, 
        PCR = cv_model2, 
        PLS = cv_model3,
        Elastic_net = cv_model4,
        GAM = cv_model5,
        MARS = SI_Final
)))#$statistics$RMSE #%>%
        # kableExtra::kable() %>%
        # kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

################################################################################

# Zadania

### 1.

# Wczytaj dane AmesHpusing (zmienna objaśniana: SalePrice). Wytrenuj model do prognozowania
# ceny transakcyjnej. Czy model jest lepszy od regresji liniowej?
install.packages('AmesHousing')
library(AmesHousing )
data("ames_raw")
help(ames_raw)
summary(ames_raw)

# Źródło danych:
# http://jse.amstat.org/v19n3/decock.pdf

################################################################################
### 2.

# Wczytaj dane biopsy (Zmienna objaśniana: Class), następnie:
data("biopsy")
help("biopsy")

# a) Przygotuj dane:

# Usunięcie nie potrzebnej zmiennej
biopsy$ID = NULL

# Przypisanie nazw kolumnom
names(biopsy) <- c("thick", "u.size", "u.shape",
                   "adhsn", "s.size", "nucl", "chrom", "n.nuc",
                   "mit", "class")

# b) Usuń braki danychUsunięcie braków danych
biopsy <- na.omit(biopsy)

# c) Narysuj wykresy każdej ze zmiennych objaśniających z objaśnianą:
gather(biopsy, variable, value, -class) %>% 
  ggplot(aes(x = class, y = value)) + 
    geom_boxplot() + facet_wrap(~ variable, ncol = 3)

# d)    Zbadaj występowanie współliniowości w danych:
library(corrplot)
corrplot.mixed(biopsy[, 1:9])

# e) Zbuduj model wykrywający typ nowotworów u pacjentów (w pakiecie caret)

################################################################################
earth.fit <- earth(class ~ ., data = biopsy,
                   pmethod = "cv",
                   nfold = 5,
                   ncross = 3,
                   degree = 1,
                   minspan = -1,
                   glm=list(family=binomial)
)

# Wyniki
summary(earth.fit)
plotd(earth.fit) # rozkład wartości prognozowanych
evimp(earth.fit) # ważność zmiennych