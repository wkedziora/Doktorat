###########################################################
#         Warsztaty Machine Learning w programie R        #
#                    Piotr Ćwiakowski                     #
#                       LabMasters                        #
#               Analiza Głównych Składowych               #
###########################################################

# install.packages('factoextra')
# install.packages('corrplot')
# install.packages('FactoMineR')
# install.packages('rgl')
# install.packages('car')
# install.packages('psych')
# install.packages('pls')
library(rgl)
library(car)
library(factoextra)
library(corrplot)
library(FactoMineR)
library(car)
library(psych)
library(pls)
library(feather)

###################################################################
## Wczytanie danych
si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather"))

names(si_factors)

si_factors_pca <- si_factors[complete.cases(si_factors$bio04) & complete.cases(si_factors$tri), c(1, 3, 13:65)]
summary(si_factors_pca)

# Na tych zmiennych pca (liczba bramek)
f.pca <- si_factors_pca[, -1]

# Analiza korelacji 
# Field et al. (2012) provide some thresholds, suggesting that no variable should have many correlations below .30, 
# or any correlation at all above .90.
cor.mat <- round(cor(f.pca), 2)
corrplot::corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt = 45)

# Klastrowanie zmiennych
corrplot::corrplot(cor.mat, type = "upper", order = "hclust", tl.col="black", tl.cex = 1, addrect = 2)

# Diagnostyka:
 
# Test Bartletta na intrakorelacje
psych::cortest.bartlett(f.pca)
 
# współczynnik KMO (Kaiser-Meyer-Olkin), powinien być powyżej 0.7
psych::KMO(cor.mat)

det(cor.mat)
 
#################################################################
# Wizualizacja i analiza wyników
# Źródło:
# https://r-posts.com/naive-principal-component-analysis/

pc1 <- psych::principal(f.pca, nfactors = length(f.pca), rotate = "none")
pc1 

plot(pc1$values, type = 'b') 

pc2 <- psych::principal(f.pca, nfactors = 5, rotate = "varimax", scores = TRUE)
pc2
pc2$loadings

# Healthcheck- We would want:
pc2$residual #Less than half of residuals with absolute values > 0.05
pc2$fit #Model fit > .9
pc2$communality #All communalities > .7

f.pca2 <- cbind(si_factors_cc, pc2$scores)
summary(f.pca2$RC1, f.pca2$RC2) 

pcr_model <- lm(SI ~ RC1 + RC2 + RC3 + RC4 + RC5, data = f.pca2)
summary(pcr_model)

# Źródło:
# http://www.sthda.com/english/wiki/cluster-analysis-in-r-unsupervised-machine-learning

obiekt.pca <- PCA(f.pca, graph = FALSE)

# Wykres łokciowy
fviz_screeplot(obiekt.pca, ncp=10, ylim = c(0, 50))
summary(obiekt.pca)

# Wyświetlmy łącznie z wartościami własnymi
fviz_eig(obiekt.pca, addlabels = TRUE, ylim = c(0, 50))

# Wykres zmiennych w pca
fviz_pca_var(obiekt.pca, col.var="steelblue") +
  theme_minimal()

# Włącznie z kontrybucją do wymiarów (korelacja)
fviz_pca_var(obiekt.pca, col.var="contrib") +
scale_color_gradient2(low="white", mid="blue", 
                      high="red", midpoint=15)+theme_bw()

# Jakość reprezentacji w wymiarach
diagnostyka <- get_pca_var(obiekt.pca)
diagnostyka

# Dysponujemy trzema charakterystykami:
# cor - korelacją zmiennych z komponentami
# cos2 - jakością reprezentacji zmiennych na komponentach
# contrib - wielkość kontrybucji zmiennej do komponentu

# Chcemy się dowiedzieć:
# a) które zmienne są reprezentowane przez który wymiar?
# b) które zmienne są skorelowane z nimi (podobnie zmieniają sie ich wartości)?
# c) które zmienne mają wysoki wkład w całkowitą informację o wymiarze?

# Oczywiście informacje te są od siebie współzależne, ale ponieważ w skład ładunku wchodzi wiele zmiennych, 
# to nie koniecznie muszą być równoznaczne.


# a) Jakość reprezentacji zmiennych:
corrplot(diagnostyka$cos2, is.corr=FALSE)
fviz_cos2(obiekt.pca, choice = "var", axes = 1:2)
fviz_pca_var(obiekt.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # Avoid text overlapping

# b) Kontrybucja:
corrplot(diagnostyka$contrib, is.corr=FALSE) 
# Kontrybucja do wymiaru PC1
fviz_contrib(obiekt.pca, choice = "var", axes = 1, top = 30)
# Kontrybucja do wymiaru PC2
fviz_contrib(obiekt.pca, choice = "var", axes = 2, top = 30)

fviz_pca_var(obiekt.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Charakterystyka wymiaru
res.desc <- dimdesc(obiekt.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
res.desc$Dim.2

# Wykres piłkarzy
fviz_pca_ind(obiekt.pca, geom="text")

# Jakość reprezentacji w wymiarach 
fviz_pca_ind(obiekt.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

# Pokolorujmy punkty wg różnych kategorii
si_factors_cc$val_kat <- cut(si_factors_cc$SI,breaks=5)

fviz_pca_ind(obiekt.pca, label = 'none', habillage=si_factors_cc$val_kat,
             addEllipses=T, ellipse.level=0.95)

# Na koniec biplot
fviz_pca_biplot(obiekt.pca,  geom = "text")

####################################################################################
# Prognozowanie wartości dla oszacowanego modelu PCA
# nowy_zbior - obiekt musi mieć takie same zmienne jak zbiór treningowy

# Wybierzmy kilka obserwacji ze zbioru treningowego i użyjmy go jako testowy.
nowy_zbior <- f[1:10,5:10]

fit <- predict(obiekt.pca, nowy_zbior)

fit$coord[,1]

# A w bazie pierwotnej..
obiekt.pca$ind$coord[1:10,1]

# Czyli skalowanie jest automatyczne.

# Więcej informacji:
browseURL('https://www.r-bloggers.com/naive-principal-component-analysis-using-r/')
browseURL('http://ucanalytics.com/blogs/principal-component-analysis-step-step-guide-r-regression-case-study-example-part-4/')
browseURL('https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/')
browseURL('http://web.missouri.edu/~huangf/data/mvnotes/Documents/pca_in_r_2.html')
browseURL('https://www.cbn.gov.ng/out/2013/sd/cbn%20jas%20volume%203%20number%202_article%203.pdf')
browseURL('https://www.r-bloggers.com/performing-principal-components-regression-pcr-in-r/')
brwoseURL('https://www.kaggle.com/rohitjain2086/breast-cancer-dataset-prediction/log')

# Ćwiczenie

# Wczytaj dane wisc_bc_data.csv (zmienna objaśniana diagnosis). Następnie:
# a. Wykonaj korelogram
# b. (0pcjonalnie) Wykorzystaj funkcje caret::findCorrelation żeby usunąć zmienne
#  skorelowane z innymi powyżej 90%.
# c) Wykonaj analizę PCA
# d) Ile komponentów wybrałbyś do analizy?
# e) Narysuj wykres biplot i pokoloruj punkty według zmiennej objaśnianej.


# Ciekawy przykład:
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

######################################################################################

linear_model222 <- lm(SI ~ ., data = obiekt.pca)

obiekt.pca$var

# prepare PCA regression

library(plyr)
library(caret)

training.samples <- si_factors_cc$SI %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- si_factors_cc[training.samples, ]
test.data <- si_factors_cc[-training.samples, ]

model0 <- lm(SI ~ . , data = train.data) #problem with perfect multicolienarity
model1 <- lm(SI ~ . -bio06, data = train.data)
model2 <- lm(SI ~ wiek_pan_pr + monthCountByTemp10 + PETDriestQuarter + topoWet + tri + bio01, 
             data = train.data)
alias(model1) # checking if there is perfcet multicolinearity

# Make predictions
predictions <- model2 %>% predict(test.data)
# Model performance
data.frame(
        RMSE = RMSE(predictions, test.data$SI),
        R2 = R2(predictions, test.data$SI)
)

car::vif(model2)

# Choose a VIF cutoff under which a variable is retained (Zuur et al. 2010 
# MEE recommends 2)
cutoff = 10

# Create function to sequentially drop the variable with the largest VIF until 
# all variables have VIF > cutoff
flag = TRUE
viftable = data.frame()
while(flag == TRUE) {
        vfit = vif(model1)
        viftable = rbind.fill(viftable, as.data.frame(t(vfit)))
        if(max(vfit) > cutoff) { model1 =
                update(model1, as.formula(paste(".", "~", ".", "-", names(which.max(vfit))))) }
        else { flag = FALSE } }
# Look at the final model
print(model1)
# And associated VIFs
print(vfit)
# And show the order in which variables were dropped
print(viftable)

summary(model1)

library(MASS)
step <- stepAIC(model1, scope = list(upper = ~., lower = ~1), direction = "both")
step$anova # display results
summary(step)
