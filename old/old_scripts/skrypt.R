library(tidyverse)
library(tmap)
# library(rgdal)
library(sp)
# library(FSA)
library(RColorBrewer)
# library(EnvStats)
library(feather)
library(GWmodel)

# data loading -----
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather"))
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))
litography_500k <- read_feather(paste0(getwd(), "/data/WMS/litography_500k.feather"))
litography_50k <- read_feather(paste0(getwd(), "/data/WMS/litography_50k.feather"))

# data wrangling -----
# ggplot(trees, aes(h)) + geom_freqpoly(binwidth = 1)
# ggplot(trees, aes(x = "", y = h)) + geom_boxplot() + coord_flip()
# ggplot(trees, aes(sample = h)) + stat_qq()
# ggplot(trees, aes(h)) + stat_ecdf(geom = "step")
# summary(trees$h, na.rm = TRUE)

# no of sample plots with different no of h measured
# trees %>%
#   filter(!is.na(h)) %>%
#   group_by(subplot_no) %>%
#   summarise(n = n_distinct(h)) %>%
#   group_by(n) %>%
#   summarise(z = n_distinct(subplot_no)) %>%
#   ggplot(aes(n, z)) +
#   geom_bar(stat = "identity")
# 
# trees %>%
#   group_by(subplot_no) %>%
#   filter(!is.na(dbh), species == "SO") %>%
#   left_join(., area, by = "subplot_no") %>%
#   filter(!is.na(h)) %>%
#   group_by(subplot_no) %>%
#   summarise(n = n_distinct(h)) %>%
#   group_by(n) %>%
#   summarise(z = n_distinct(subplot_no)) %>%
#   ggplot(aes(n, z)) +
#   geom_bar(stat = "identity")

### Site Index calculations ----------------

values <- tibble(
  b1 = 1.381,
  b3 = 32.947,
  b2 = 4679.9
)

# main assumption is that I only count in Pine trees
# trees %>%
#   group_by(plot_no, subplot_no) %>%
#   filter(species == "SO") %>%
#   summarise(H = mean(h, na.rm = TRUE), # średnia wysokość ważona pierśnicą
#     wiek = mean(age), # wiek powierzchni
#     SI = H * ((100 ^ values$b1) * ((wiek ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + 
#          (2 * values$b2 * H) / (wiek ^ values$b1)) ^ 0.5) + values$b2)) / ((wiek ^ values$b1) * 
#          ((100 ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + (2 * values$b2 * H) /
#          (wiek ^ values$b1)) ^ 0.5) + values$b2))) %>% 
#   filter(complete.cases(SI)) %>% # muszę w jakiś sposób usunąć wpisy gdzie SO nie jest panująca
#   dplyr::mutate(kw = cut(wiek, breaks=seq(0, 260, by=20))) %>%
#   arrange(desc(SI)) -> site_index_raw

trees %>%
  filter(species == "SO") %>%
  na.omit(h) %>%
  dplyr::mutate(H = h, wiek = age,
    SI_raw = H * ((100 ^ values$b1) * ((wiek ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + 
         (2 * values$b2 * H) / (wiek ^ values$b1)) ^ 0.5) + values$b2)) / ((wiek ^ values$b1) * 
         ((100 ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + (2 * values$b2 * H) /
         (wiek ^ values$b1)) ^ 0.5) + values$b2))) %>% 
  group_by(plot_no, subplot_no) %>%
  dplyr::summarise(SI = mean(SI_raw)) %>%
  filter(complete.cases(SI)) -> site_index_raw_2

# main assumption is that on small areas <200m^2 is not enough trees to get proper measurments
site_index <- dplyr::inner_join(sites, site_index_raw_2, by = "subplot_no", suffix = c("", ".y")) %>% 
  dplyr::left_join(., area, by = "subplot_no") %>% 
  filter(area >= 200)
 
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

site_index %>% mutate(z_mean = as.vector(scale(SI, center = TRUE, scale = FALSE)), z_sd = scale_this(SI)) -> site_index

### backup write site_index ------------------------------------------------------------------------------------
# write_feather(site_index, paste0(getwd(), "/data/WISL/site_index.feather"))
# using fst to read/write operations?

# levels(site_index$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

# ggplot(site_index, aes(SI)) + geom_freqpoly(binwidth = 1)
# ggplot(site_index, aes(x = "", y = SI)) + geom_boxplot()
# ggplot(site_index, aes(sample = SI)) + stat_qq()
# ggplot(site_index, aes(SI)) + stat_ecdf(geom = "step")
# summary(site_index$SI, na.rm = TRUE)

# ggplot(data = site_index, aes(x = kw, y = SI)) + geom_boxplot()

site_index_gps <- dplyr::left_join(site_index, gps, by = "plot_no") #%>% na.omit()

### backup write/load site_index_gps ---------------------------------------------------------------------------------
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

coordinates(site_index_gps) <- ~ lon + lat #adding sptial relationship
proj4string(site_index_gps) <- "+init=epsg:4326" #adding WGS84 projection

# site index map plotting -----
data(Europe, rivers)
# vistula <- subset(rivers, name == "Vistula")
Poland <- subset(Europe, name == "Poland")
plot_map <- function(shape, feature) {
  tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    # tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    tm_shape(shape) + tm_dots(col = feature, size = 0.05, palette = "PiYG", n = 5, auto.palette.mapping = FALSE) +
    tm_style_white(legend.position = c("left", "bottom"))
}

plot_map(site_index_gps, "SI")


### local Moran's I ---------------------------------------------------------------------------------------------------
library(spatstat)
library(spdep)
library(maptools)

site_index_moran <- site_index_gps

nghbr_1 <- dnearneigh(site_index_moran, 0, 1, longlat = TRUE)
moran_1 <- localmoran(site_index_moran$SI, nb2listw(nghbr_1, zero.policy = TRUE), na.action = na.omit)

nghbr_5 <- dnearneigh(site_index_moran, 0, 5, longlat = TRUE)
moran_5 <- localmoran(site_index_moran$SI, nb2listw(nghbr_5, zero.policy = TRUE), na.action = na.omit)

site_index_moran@data <- data.frame(site_index_moran@data, as.data.frame(moran_1), as.data.frame(moran_5))

plot_map(site_index_moran, "SI")
plot_map(site_index_moran, "z_mean")
plot_map(site_index_moran, "z_sd")
plot_map(site_index_moran, "Ii")
plot_map(site_index_moran, "Ii.1")

### raster WorldClim/envirem data --------------------------------------------------------------------------------
library(raster)
worldclim_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\WorldClim", pattern='\\.tif$', full.names = TRUE)
worldclim <- raster::stack(worldclim_data)

envirem_data <- list.files(path = "D:\\Praca\\Badania\\Doktorat\\data\\envirem", pattern='\\.tif$', full.names = TRUE)
envirem <- raster::stack(envirem_data)

site_index_environ <- as_tibble(data.frame(coordinates(site_index_gps),
                                          site_index_gps@data[,c(1:9, 11:15)], 
                                          raster::extract(worldclim, site_index_gps),
                                          raster::extract(envirem, site_index_gps)))
summary(site_index_environ)

### backup write site_index_environ ---------------------------------------------------------------------------------
# write_feather(site_index_environ, paste0(getwd(), "/data/WISL/site_index_environ.feather"))
# site_index_environ <- read_feather(paste0(getwd(), "/data/WISL/site_index_environ.feather"))

site_index_environ_cc <- site_index_environ[complete.cases(site_index_environ$bio_04) & complete.cases(site_index_environ$topoWet),]
coordinates(site_index_environ_cc) <- ~ lon + lat
proj4string(site_index_environ_cc) <- "+init=epsg:4326" #adding WGS84 projection
site_index_environ_cc_2180 <- spTransform(site_index_environ_cc, "+init=epsg:2180")

linear_model <- lm(SI ~ bio_04 + bio_05 + bio_12 + habitat, data = site_index_environ) #u Sochy R = 0,29
# linear_model <- lm(SI ~ ., data = site_index_environ) #u Sochy R = 0,29
summary(linear_model)

library(mgcv)
gam_model <- mgcv::gam(SI ~ s(bio_04) + s(bio_05) + s(bio_12) + habitat, data = site_index_environ) #u Sochy R = 0,29
summary(gam_model)

data_resid <- data.frame(coordinates(site_index_environ_cc), site_index_environ_cc@data)
# data_resid <- data_resid[complete.cases(data_resid$habitat),]
# data_resid <- data_resid[complete.cases(data_resid$bio_04),]0
data_resid[, "resid_lm"] <- as.double(linear_model$residuals)
data_resid[, "resid_gam"] <- as.double(gam_model$residuals)
coordinates(data_resid) <- ~ lon + lat
proj4string(data_resid) <- "+init=epsg:4326" #adding WGS84 projection

resid1 <- plot_map(data_resid, "resid_lm")
resid2 <- plot_map(data_resid, "resid_gam")
tmap_arrange(resid1, resid2, asp = NA)

### GW Summary Statistics -------------------------------------------------------------------------------------------------

localstats1 <- gwss(site_index_environ_cc_2180, vars = c("SI", "bio_04", "topoWet"), bw = 50000)

data(Europe, rivers)
Poland <- subset(Europe, name == "Poland")
Poland_2180 <- spTransform(Poland, "+init=epsg:2180")

quick.map <- function(spdf, var, legend.title, color) {
  x <- spdf@data[,var]
  cut.vals <- pretty(x)
  x.cut <- cut(x, cut.vals)
  cut.levels <- levels(x.cut)
  cut.band <- match(x.cut, cut.levels)
  colors <- rev(brewer.pal(length(cut.levels), color))
  par(mar = c(1, 1, 1, 1))
  plot(Poland_2180, col = 'olivedrab', bg = 'lightblue1')
  # title(main.title)
  plot(spdf, add = TRUE, col = colors[cut.band], pch = 16)
  legend('topleft', cut.levels, col = colors, pch = 16, bty = 'n', title = legend.title)
}

quick.map(localstats1$SDF, "SI_LM", "Site Index", "Purples")
quick.map(localstats1$SDF, "bio_04_LM", "bio 04 mean", "Purples")
quick.map(localstats1$SDF, "topoWet_LM", "topoWet mean", "Purples")
quick.map(localstats1$SDF, "Corr_SI.bio_04", expression(rho), "Purples")
quick.map(localstats1$SDF, "Corr_SI.topoWet", expression(rho), "Purples")

### saving data for shiny GW Local Statistics ----------------------------------------------------------------------------

saveRDS(localstats1, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat.rds")
saveRDS(Poland_2180, file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/Poland_2180.rds")
localstats1 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/localstat.rds")
Poland_2180 <- readRDS(file = "D:/Praca/Badania/Doktorat/shiny/SI_localstatistics/data/Poland_2180.rds")

### GWmodel ---------------------------------------------------------------------------------------------------------------
data_gw <- as_tibble(data.frame(coordinates(site_index_environ_cc), site_index_environ_cc@data)) %>% dplyr::select(-c(3, 4, 5, 6, 7, 11, 12, 13, 15, 16, 17)) %>% na.omit()
coordinates(data_gw) <- ~ lon + lat
proj4string(data_gw) <- "+init=epsg:4326" #adding WGS84 projection

## if projected CRS is needed
data_gw_2180 <- spTransform(data_gw, "+init=epsg:2180")

bandwidth_fixed <- bw.gwr(SI ~ bio_04 + topoWet, data = data_gw_2180, kernel = "gaussian", approach = "AICc")
bandwidth_fixed_cv <- bw.gwr(SI ~ bio_04 + topoWet, data = data_gw_2180, kernel = "gaussian", approach = "CV")
bandwidth_adaptive <- bw.gwr(SI ~ bio_04 + topoWet, data = data_gw_2180, kernel = "gaussian", approach = "AICc", adaptive = TRUE, dMat = dist)
bandwidth_adaptive_CV <- bw.gwr(SI ~ bio_04 + topoWet, data = data_gw_2180, kernel = "gaussian", approach = "CV", adaptive = TRUE, dMat = dist)
# bandwidth <- 6.10312770599552 #longlat
bandwidth <- 6621.596


dist <- gw.dist(dp.locat = coordinates(data_gw_2180))

gwr <- gwr.basic(SI ~ bio_04 + topoWet, data = data_gw_2180, bw = bandwidth, kernel = "gaussian", dMat = dist) # gwr r-sqr 0.00049
gwr2 <- gwr.basic(SI ~ bio_04, data = data_gw_2180, bw = bandwidth * 2, kernel = "gaussian", dMat = dist) 
gwr3 <- gwr.basic(SI ~ bio_04, data = data_gw_2180, bw = bandwidth * 10, kernel = "gaussian", dMat = dist) 
gwr4 <- gwr.basic(SI ~ bio_04 + bio_05 + bio_12 + habitat, data = data_gw_2180, bw = bandwidth, kernel = "gaussian", dMat = dist)
gwr5 <- gwr.basic(SI ~ bio_04 + bio_05 + bio_12 + habitat, data = data_gw_2180, bw = bandwidth * 2, kernel = "gaussian", dMat = dist)
gwr6 <- gwr.basic(SI ~ bio_04 + bio_05 + bio_12 + habitat, data = data_gw_2180, bw = bandwidth * 10, kernel = "gaussian", dMat = dist)
gwr7 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth, kernel = "gaussian", dMat = dist)
gwr8 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth * 2, kernel = "gaussian", dMat = dist) 
gwr9 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth * 10, kernel = "gaussian", dMat = dist) 
gwr10 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth, kernel = "gaussian", dMat = dist) # not working?
gwr11 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth * 2, kernel = "gaussian", dMat = dist)  # not working?
gwr12 <- gwr.basic(SI ~ ., data = data_gw_2180, bw = bandwidth * 10, kernel = "gaussian", dMat = dist) # not working?




### old things -------------------------------------------------------------------------------------------------------------

  wynikGPS %>%
  select(point_no = nr_punktu, subpoint_no = nr_podpow, lat = szerokosc, long = dlugosc, SI, tsl, ukszt_ter) -> wynikGPS
  
write.table(wynikGPS, "resultGPS.txt", sep="\t", row.names=FALSE)

# results for Poland -----

site_index %>% summarise(srednia = mean(SI), 
                    mediana = median(SI),
                    IQR = IQR(SI)) -> srednieSI

# histogram dla Site Index
ggplot(data = site_index, aes(x = SI)) +
  geom_histogram(binwidth = 2) + 
  geom_freqpoly() +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
  theme_bw() +
  scale_x_continuous(limits = c(5, 45))

# punkty dla Site Index
ggplot(data = site_index, aes(x = wiek, y = SI)) +
  geom_point() + 
  labs(x = "wiek", y = "wskaźnik bonitacji (SI)") +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_continuous(limits = c(0, 60))

# histogram skumulowany
h <- hist(site_index$SI, breaks = seq(6, 58, by=4))
h$counts <- cumsum(h$counts)
plot(h)

# wynik w zależności od wieku
# xlabs <- paste(levels(wynik$kw),"\n(N=",table(wynik$kw),")",sep="") # stara funkcja dopisująca etykiety
ggplot(data = na.omit(site_index), aes(x = kw, y = SI)) + 
  geom_boxplot(notch = TRUE) + 
  stat_n_text() +
  theme_bw() +
  labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

# test Kruskala-Wallisa
kruskal.test(SI ~ kw, site_index)

pairwise.wilcox.test(site_index$SI, site_index$kw, p.adjust.method = "BH")
dunnTest(SI ~ kw, data = wynik, method="bh")

# results for regions -----

site_index %>% 
  group_by(kraina) %>%
  summarise(srednia = mean(SI),
            mediana = median(SI),
            IQR = IQR(SI)) -> srednieSIkrainami

# histogram w podziale na krainy dla Site Index
site_index %>%
  group_by(kraina) %>%
  summarise(n = paste("n =", length(kraina))) -> kraina.labs

ggplot(data = site_index, aes(x = SI)) +
  geom_histogram(binwidth = 4) +
  facet_wrap(~kraina) +
  geom_text(data = kraina.labs, aes(x=50, y=1250, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) +
  theme_bw() +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
  scale_x_continuous(limits = c(0, 60))

# mapa ze średnim SI dla krain
# myCols <- adjustcolor(colorRampPalette(brewer.pal(n=8, 'Greens'))(100), .85) #kolorki na przyszłość
krainy.shp <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")
krainy2.shp <- sp::merge(krainy.shp, srednieSIkrainami, by="kraina")
spplot(krainy2.shp, zcol="mediana")

# wynik w zależności od krainy
# xlabs <- paste(sort(unique(site_index$kraina)),"\n(N=",table(site_index$kraina),")",sep="") # stara funkcja dopisująca etykiety
ggplot(data = site_index, aes(x = as.factor(kraina), y = SI)) + 
  geom_boxplot(notch = TRUE) + 
  stat_n_text() +
  theme_bw() + 
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

# test Kruskala-Wallisa
kruskal.test(SI ~ kraina, site_index)
pairwise.wilcox.test(site_index$SI, site_index$kraina, p.adjust.method = "BH") # test post-hoc
dunnTest(SI ~ kraina, data = site_index, method="bh") # test post-hoc

# wykres w podziale na krainy w zależności od wieku
site_index %>%
  group_by(kw) %>%
  summarise(n = paste("n =", length(kw))) -> kw.labs

ggplot(data = site_index, aes(x = kraina, y = SI, group = kraina)) + 
  geom_boxplot(notch = TRUE, varwidth = TRUE)  + 
  geom_text(data = kw.labs, aes(x=7, y=60, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) +
  # stat_n_text() + 
  facet_wrap(~kw) + 
  theme_bw() + 
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

## results for Forest Inspectorates -----

site_index %>% 
  group_by(kodn) %>%
  summarise(mediana = median(SI)) -> srednieSInadles

site_index %>% 
  group_by(kw, kodn) %>%
  summarise(mediana = median(SI)) %>%
  tidyr::spread(key = kw, value = mediana) -> srednieSInadlesKW

site_index %>% 
  group_by(kw) %>%
  summarise(mediana = median(SI)) -> srednieSInadlesKW

# mapa ze średnim SI dla nadleśnictw
nadles.shp <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "nadles")
nadles2.shp <- sp::merge(nadles.shp, srednieSInadles, by.x="KODN", by.y = "kodn")
nadles3.shp <- sp::merge(nadles.shp, srednieSInadlesKW, by.x="KODN", by.y = "kodn")
# colnames(nadles3.shp@data)[10] <- c("VI")
spplot(nadles2.shp, zcol = "mediana", at = seq(0, 40, by = 4))
spplot(nadles3.shp, zcol = "I", at = seq(0, 60, by = 4))
spplot(nadles3.shp, zcol = "II", at = seq(0, 60, by = 4))
spplot(nadles3.shp, zcol = "III", at = seq(0, 60, by = 4))
spplot(nadles3.shp, zcol = "IV", at = seq(0, 60, by = 4))
spplot(nadles3.shp, zcol = "V", at = seq(0, 60, by = 4))
spplot(nadles3.shp, zcol = "VI", at = seq(0, 60, by = 4))

# results for habitats -----

site_index %>% 
  group_by(tsl) %>%
  summarise(srednia = mean(SI),
            mediana = median(SI),
            IQR = IQR(SI)) -> srednieSItsl

# histogram w podziale na RDLP dla Site Index
SI <- ggplot(data = site_index, aes(x = SI)) 
SI  + geom_histogram() + facet_wrap(~tsl)

# wykres w podziale na tsl w zależności od wieku
ggplot(data = subset(site_index, !is.na(tsl)), aes(x = wiek, y = SI)) + geom_point() + facet_wrap(~tsl)

dane2 %>%
  filter(nr_podpow == 60083401)

dane2 %>%
  filter(nr_podpow == 92015402)

# jak zmieniała się wysokość na powierzchniach dla poszczególnych drzew i dla średniej H100
#

#sprawdzenie które powierzchnie mają bardzo zróżnicowany wiek drzew
dane2 %>% 
  group_by(nr_podpow) %>% 
  summarise(wiek_powierzchni = mean(wiek_pan_pr), wiek_drzew = mean(wiek)) %>% 
  filter(wiek_powierzchni != wiek_drzew)

#sprawdzenie czy między cyklami zmieniono wiek powierzchni próbnej
dane %>%
  group_by(nr_cyklu, nr_podpow) %>%
  summarise(wiek = mean(wiek_pan_pr, rm.na = TRUE)) %>%
  spread(nr_cyklu, wiek) 

# liczymy na medianach
# dorzucamy testy kruskala wallisa oraz test post hoc miedzy krainami
# w krainie dla poszczególnych klasach wieku

# klasy wieku i boxploty dla każdej w kraju, w poszczególnych krainach
# oddzielne wykresy dla poszcególnych krain w rozbiciu na klasy wieku (dla V kw. pokazać boxplot dla krain)

# linie trendu do siedlisk plus porównać linie trendu na jednym wykresie

# rozstęp większy niż 20 lat - zaingerować
# najniższe SI - sprawdzić gdzie występuje
# histogram i histogram skumulowany - ogólnie dla Polski i w rozbiciu na krainy oraz na TSL
# zależność między wiekiem a SI - o gólnie i w krainach (jak sprawdza się w każdej z nich?) 

# średni SI dla nadleśnictwa w kraju - 
# mapka dla całego kraju w podziale na nadleśnictwa plus podział na młodsze i starsze? w klasach wieku?

# prezentacja na semianrium - użyć starej prezentacji: zmodyfikować wykres prezentujący SI
#  - historia SI
#  - metodyka - wzór dynamiczny i statyczny
#  - pierwsze wyniki
#      - dwie grupy: wielkości bonitacji w różnych podziałach
#      - badanie związku z różnymi elementami środowiska

# różne wieki na powierzchni próbnej w drzewach z pomierzoną wysokością - co z tym fantem?

### solved problems -----

# co z przestojami w kodzie "war" - przestoje maja inny kod "war" = 10 lub 11

# nowy wzór od Sochy:
# sprawdzić jak ma się zależność od wzoru Sochy - celowość tworzenia modeli
# uzyć krainy gdzie Socha badał w południowej Polsce żeby to sprawdzić i porównać

### future ideas -----

# porównanie SI między dwoma cyklami? różnice w bonitacjach na tej samej powierzchni
# analiza różnic
# błąd pomiaru wysokości:
# w 5 lat mały przyrost - możliwość dużych błędów przy małych wzrostach
# 

