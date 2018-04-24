library(tidyverse)
library(tmap)
library(rgdal)
library(sp)
# library(FSA)
library(RColorBrewer)
# library(EnvStats)
library(feather)
library(GWmodel)
library(EnvStats)
library(FSA)

## ----- data_loading -------------------------------------------------------------------------
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather")) ### comment one
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))
litography_500k <- read_feather(paste0(getwd(), "/data/WMS/litography_500k.feather"))
litography_50k <- read_feather(paste0(getwd(), "/data/WMS/litography_50k.feather"))

## @knitr

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
## ----- values
values <- tibble(
  b1 = 1.381,
  b3 = 32.947,
  b2 = 4679.9
)

## @knitr

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

## ------ SI --------------------------------------------------------------------------------------------

# ### SI w wersji jednolitej dla całej Polski
# trees %>%
#   filter(species == "SO") %>%
#   na.omit(h) %>%
#   dplyr::mutate(H = h, wiek = age, 
#                 SI_raw = H * ((100 ^ values$b1) * ((wiek ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + 
#                 (2 * values$b2 * H) / (wiek ^ values$b1)) ^ 0.5) + values$b2)) / ((wiek ^ values$b1) * 
#                 ((100 ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + (2 * values$b2 * H) /
#                 (wiek ^ values$b1)) ^ 0.5) + values$b2))) %>%
#   group_by(plot_no, subplot_no) %>%
#   dplyr::summarise(SI = mean(SI_raw)) %>%
#   filter(complete.cases(SI)) -> site_index_raw_2

# Si w rozbiciu na krainy
trees %>%
  filter(species == "SO") %>%
  na.omit(h) %>%
  dplyr::mutate(b1 = case_when(region == 1 ~ 1.281,
                              region == 2 ~ 1.503,
                              region == 3 ~ 1.326,
                              region == 4 ~ 1.408,
                              region == 5 ~ 1.418,
                              region == 6 ~ 1.460,
                              region == 7 ~ 1.317,
                              region == 8 ~ 1.317),
                b3 = case_when(region == 1 ~ 32.680,
                              region == 2 ~ 39.750,
                              region == 3 ~ 26.720,
                              region == 4 ~ 22.520,
                              region == 5 ~ 37.240,
                              region == 6 ~ 15.140,
                              region == 7 ~ 29.890,
                              region == 8 ~ 29.890),
                b2 = case_when(region == 1 ~ 8813,
                              region == 2 ~ 2582,
                              region == 3 ~ 9706,
                              region == 4 ~ 7520,
                              region == 5 ~ 2157,
                              region == 6 ~ 1009,
                              region == 7 ~ 1420,
                              region == 8 ~ 1420),
                H = h, 
                wiek = age,
                SI_raw = H*((100^b1)*((wiek^b1)*(H-b3+(((H-b3)^2) + 
         (2 * b2 * H) / (wiek ^ b1)) ^ 0.5) + b2)) / ((wiek ^ b1) * 
         ((100 ^ b1) * (H - b3 + (((H - b3) ^ 2) + (2 * b2 * H) /
         (wiek ^ b1)) ^ 0.5) + b2))) %>% 
  group_by(plot_no, subplot_no) %>%
  dplyr::summarise(SI = mean(SI_raw)) %>%
  filter(complete.cases(SI)) -> site_index_raw_2

sites %>% dplyr::mutate(kw = cut(plot_age, breaks=seq(0, 260, by=20)),
                        humidity = factor(as.numeric(levels(habitat))[habitat]%%10),
                        fertility = factor(as.numeric(levels(habitat))[habitat]%/%10),
                        region = factor(region)) -> sites
levels(sites$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

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

t <- theme_bw()

## ----- POLSKA ------------------------------------------------------------------------------------------
site_index %>%  summarize(n = n(),
                          min = min(SI), 
                          q1 = quantile(SI, 0.25), 
                          median = median(SI), 
                          mean = mean(SI),  
                          sd = sd(SI),
                          q3 = quantile(SI, 0.75),
                          max = max(SI),
                          skewness = e1071::skewness(SI),
                          kurtosis = e1071::kurtosis(SI))

# histogram dla Site Index
ggplot(data = site_index, aes(x = SI)) +
  geom_histogram(binwidth = 1) +
  geom_freqpoly(binwidth = 1) +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
  scale_x_continuous(limits = c(5, 45)) + t

ggplot(site_index, aes(x = "", y = SI)) + geom_boxplot() + t
ggplot(site_index, aes(sample = SI)) + stat_qq() + t 
ggplot(site_index, aes(SI)) + stat_ecdf(geom = "step") + t 

## ----- krainy ----------------------------------------------------------------------------------------
### podpisać krainy na wykresie
site_index %>% group_by(region) %>% summarize(n = n(),
                                              min = min(SI), 
                                              q1 = quantile(SI, 0.25), 
                                              median = median(SI), 
                                              mean = mean(SI), 
                                              sd = sd(SI),
                                              q3 = quantile(SI, 0.75),
                                              max = max(SI),
                                              skewness = e1071::skewness(SI),
                                              kurtosis = e1071::kurtosis(SI)) %>%
  dplyr::mutate(kraina = factor(region)) -> srednieSIkrainami

site_index %>%
  group_by(region) %>%
  summarise(n = paste("n =", length(region))) -> region.labs

ggplot(data = site_index, aes(x = SI)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~region, ncol = 2) +
  geom_text(data = region.labs, aes(x=38, y=300, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") + t 

ggplot(data = site_index, aes(x = region, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text() +
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ region, site_index)
pairwise.wilcox.test(site_index$SI, site_index$region, p.adjust.method = "BH") # test post-hoc
dunnTest(SI ~ region, data = site_index, method="bh") # test post-hoc

## ----- klasy_wieku ----------------------------------------------------------------------------------------
site_index %>% group_by(kw) %>% summarize(n = n(),
                                              min = min(SI), 
                                              q1 = quantile(SI, 0.25), 
                                              median = median(SI), 
                                              mean = mean(SI),  
                                              q3 = quantile(SI, 0.75),
                                              max = max(SI),
                                              skewness = e1071::skewness(SI),
                                              kurtosis = e1071::kurtosis(SI))

ggplot(data = na.omit(site_index), aes(x = kw, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text() +
  labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ kw, site_index)
pairwise.wilcox.test(site_index$SI, site_index$kw, p.adjust.method = "BH")
dunnTest(SI ~ kw, data = site_index, method="bh")

## ----- zyznosc ----------------------------------------------------------------------------------------
### podpisać żyzność na wykresie
site_index %>% group_by(fertility) %>% summarize(n = n(),
                                          min = min(SI), 
                                          q1 = quantile(SI, 0.25), 
                                          median = median(SI), 
                                          mean = mean(SI),  
                                          sd = sd(SI),
                                          q3 = quantile(SI, 0.75),
                                          max = max(SI),
                                          skewness = e1071::skewness(SI),
                                          kurtosis = e1071::kurtosis(SI))
ggplot(data = site_index, aes(x = fertility, y = SI)) + geom_point(alpha=0.3, color="tomato", position = "jitter") + geom_boxplot(alpha = 0) 

## ----- wilgotnosc ----------------------------------------------------------------------------------------
### podpisać wilgotność
site_index %>% group_by(humidity) %>% summarize(n = n(),
                                          min = min(SI), 
                                          q1 = quantile(SI, 0.25), 
                                          median = median(SI), 
                                          mean = mean(SI),  
                                          sd = sd(SI),
                                          q3 = quantile(SI, 0.75),
                                          max = max(SI),
                                          skewness = e1071::skewness(SI),
                                          kurtosis = e1071::kurtosis(SI))
ggplot(data = site_index, aes(x = humidity, y = SI)) + geom_point(alpha=0.3, color="tomato", position = "jitter") + geom_boxplot(alpha = 0)

## ----- siedlisko ----------------------------------------------------------------------------------------
### podpisać siedlisko, drugi wykres z malejącą mesdianą
site_index %>% group_by(habitat) %>% summarize(n = n(),
                                          min = min(SI), 
                                          q1 = quantile(SI, 0.25), 
                                          median = median(SI), 
                                          mean = mean(SI), 
                                          sd = sd(SI),
                                          q3 = quantile(SI, 0.75),
                                          max = max(SI),
                                          skewness = e1071::skewness(SI),
                                          kurtosis = e1071::kurtosis(SI))
ggplot(data = site_index, aes(x = habitat, y = SI)) + geom_point(alpha=0.3, color="tomato", position = "jitter") + geom_boxplot(alpha = 0)

ggplot(data = site_index, aes(x = region, y = SI)) + geom_point(alpha=0.3, color="tomato", position = "jitter") + geom_boxplot(alpha = 0) + facet_wrap(~kw)
ggplot(data = site_index, aes(x = kw, y = SI)) + geom_point(alpha=0.3, color="tomato", position = "jitter") + geom_boxplot(alpha = 0) + facet_wrap(~region) 

## @knitr

site_index_gps <- dplyr::left_join(site_index, gps, by = "plot_no") #%>% na.omit()

## @knitr

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
    tm_shape(shape) + tm_dots(col = feature, size = 0.05, palette = "BrBG", n = 5, auto.palette.mapping = FALSE) +
    tm_style_white(legend.position = c("left", "bottom"))
}

plot_map(site_index_gps, "SI")

# mapa ze średnim SI dla krain
# myCols <- adjustcolor(colorRampPalette(brewer.pal(n=8, 'Greens'))(100), .85) #kolorki na przyszłość
krainy.shp <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")
krainy2.shp <- sp::merge(krainy.shp, as.data.frame(srednieSIkrainami), by = "kraina")
tm_shape(krainy2.shp) + tm_polygons("median", palette="RdYlGn") + tm_legend(position = c("left", "bottom"))


# wykres w podziale na krainy w zależności od wieku
site_index %>%
  group_by(kw) %>%
  summarise(n = paste("n =", length(kw))) -> kw.labs

ggplot(data = site_index, aes(x = region, y = SI, group = region)) +
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

# #sprawdzenie które powierzchnie mają bardzo zróżnicowany wiek drzew
# dane2 %>%
#   group_by(nr_podpow) %>%
#   summarise(wiek_powierzchni = mean(wiek_pan_pr), wiek_drzew = mean(wiek)) %>%
#   filter(wiek_powierzchni != wiek_drzew)
# 
# #sprawdzenie czy między cyklami zmieniono wiek powierzchni próbnej
# dane %>%
#   group_by(nr_cyklu, nr_podpow) %>%
#   summarise(wiek = mean(wiek_pan_pr, rm.na = TRUE)) %>%
#   spread(nr_cyklu, wiek)

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

