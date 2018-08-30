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

## function for scaling SI
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
} ## @knitr

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
) ## @knitr

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
                b3 = case_when(region == 1 ~ 32.68,
                               region == 2 ~ 39.75,
                               region == 3 ~ 26.72,
                               region == 4 ~ 22.52,
                               region == 5 ~ 37.24,
                               region == 6 ~ 15.14,
                               region == 7 ~ 29.89,
                               region == 8 ~ 29.89),
                b2 = case_when(region == 1 ~ 8813,
                               region == 2 ~ 2582,
                               region == 3 ~ 9706,
                               region == 4 ~ 7520,
                               region == 5 ~ 2157,
                               region == 6 ~ 10090,
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

sites %>%  dplyr::filter(plot_species == "SO") %>%
  dplyr::mutate(habitat = as.integer(levels(habitat))[habitat],
                fertility = case_when(habitat %in% c(11, 12, 13, 14) ~ "B",
                                      habitat %in% c(22, 23, 24) ~ "BM",
                                      habitat %in% c(32, 33, 34) ~ "LM",
                                      habitat %in% c(42, 43, 44, 45, 46) ~ "L",
                                      habitat %in% c(55, 56) ~ "BMwyż",
                                      habitat %in% c(61, 62) ~ "LMwyż",
                                      habitat %in% c(63, 64, 65, 66) ~ "Lwyż",
                                      habitat %in% c(71, 72, 73) ~ "BG",
                                      habitat %in% c(81, 82, 83) ~ "BMG",
                                      habitat %in% c(91, 92) ~ "LMG",
                                      habitat %in% c(93, 94, 95, 96) ~ "LG"),
                humidity = case_when(habitat %in% c(11) ~ "s",
                                     habitat %in% c(12, 22, 32, 42, 55, 61, 63, 71, 81, 91, 93, 18) ~ "ś",
                                     habitat %in% c(13, 23, 33, 43, 56, 62, 64, 72, 82, 92, 94) ~ "w",
                                     habitat %in% c(14, 24, 34, 44, 73, 83) ~ "b",
                                     habitat %in% c(45, 46, 65, 66, 95, 96) ~ "z"),
                humidity2 = case_when(habitat %in% c(11) ~ "Ns",
                                      habitat %in% c(12, 22, 32, 42) ~ "Nś",
                                      habitat %in% c(13, 23, 33, 43) ~ "Nw",
                                      habitat %in% c(14, 24, 34, 44) ~ "Nb",
                                      habitat %in% c(45, 46) ~ "Nz",
                                      habitat %in% c(55, 61, 63) ~ "Wś",
                                      habitat %in% c(56, 62, 64) ~ "Ww",
                                      habitat %in% c(65, 66) ~ "Wz",
                                      habitat %in% c(71, 81, 91, 93) ~ "Gś",
                                      habitat %in% c(72, 82, 92, 94) ~ "Gw",
                                      habitat %in% c(73, 83) ~ "Gb",
                                      habitat %in% c(95, 96) ~ "Gz",
                                      habitat %in% c(18) ~ "WGś"),
                habitat = case_when(habitat == 11 ~ "Bs",
                                    habitat == 12 ~ "Bśw",
                                    habitat == 13 ~ "Bw",
                                    habitat == 14 ~ "Bb",
                                    habitat == 18 ~ "BWG",
                                    habitat == 22 ~ "BMśw",
                                    habitat == 23 ~ "BMw",
                                    habitat == 24 ~ "BMb",
                                    habitat == 32 ~ "LMśw",
                                    habitat == 33 ~ "LMw",
                                    habitat == 34 ~ "LMb",
                                    habitat == 42 ~ "Lśw",
                                    habitat == 43 ~ "Lw",
                                    habitat == 44 ~ "Ol",
                                    habitat == 45 ~ "OlJ",
                                    habitat == 46 ~ "Lł",
                                    habitat == 55 ~ "BMwyżśw",
                                    habitat == 56 ~ "BMwyżw",
                                    habitat == 61 ~ "LMwyżśw",
                                    habitat == 62 ~ "LMwyżw",
                                    habitat == 63 ~ "Lwyżśw",
                                    habitat == 64 ~ "Lwyżw",
                                    habitat == 65 ~ "OlJwyż",
                                    habitat == 66 ~ "Lłwyż",
                                    habitat == 71 ~ "BGśw",
                                    habitat == 72 ~ "BGw",
                                    habitat == 73 ~ "BGb",
                                    habitat == 81 ~ "BMGśw",
                                    habitat == 82 ~ "BMGw",
                                    habitat == 83 ~ "BMGb",
                                    habitat == 91 ~ "LMGśw",
                                    habitat == 92 ~ "LMGw",
                                    habitat == 93 ~ "LGśw",
                                    habitat == 94 ~ "LGw",
                                    habitat == 95 ~ "LłG",
                                    habitat == 96 ~ "OlJG"),
                kw = cut(plot_age, breaks=seq(0, 260, by=20)),
                region = factor(region),
                fertility = ordered(fertility, levels = c("B", "BM", "LM", "L", "BMwyż", "LMwyż", "Lwyż", "BG", "BMG", "LMG", "LG")),
                humidity = ordered(humidity, levels = c("s", "ś", "w", "b", "z")),
                humidity2 = ordered(humidity2, levels = c("Ns", "Nś", "Nw", "Nb", "Nz","Wś", "Ww", "Wz", "Gś", "Gw", "Gb", "Gz", "WGś")),
                habitat = ordered(habitat, levels = c("Bs", "Bśw", "Bw", "Bb", "BMśw", "BMw", "BMb", "LMśw", "LMw", "LMb", "Lśw", "Lw", 
                                                      "Ol", "OlJ", "Lł",  "BMwyżśw", "BMwyżw", "LMwyżśw", "LMwyżw", "Lwyżśw", "Lwyżw", 
                                                      "OlJwyż", "Lłwyż", "BGśw", "BGw", "BGb", "BMGśw", "BMGw", "BMGb", "LMGśw", "LMGw", 
                                                      "LGśw", "LGw", "OlJG", "LłG", "BWG"))) -> sites

levels(sites$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

# main assumption is that on small areas <200m^2 is not enough trees to get proper measurments
site_index <- dplyr::inner_join(sites, site_index_raw_2, by = "subplot_no", suffix = c("", ".y")) %>% 
  dplyr::left_join(., area, by = "subplot_no") %>% 
  filter(area >= 200)

site_index %>% mutate(z_mean = as.vector(scale(SI, center = TRUE, scale = FALSE)), z_sd = scale_this(SI)) -> site_index

### backup write site_index ------------------------------------------------------------------------------------
# write_feather(site_index, paste0(getwd(), "/data/WISL/site_index.feather"))
# using fst to read/write operations?

t <- theme_bw()

### LICZBA MIEJSC PO PRZECINKU!!!

## ----- POLSKA ------------------------------------------------------------------------------------------
site_index %>%  summarize(n = n(),
                          min = min(SI), 
                          q1 = quantile(SI, 0.25), 
                          median = median(SI), 
                          mean = mean(SI),  
                          sd = sd(SI),
                          IQR = IQR(SI),
                          q3 = quantile(SI, 0.75),
                          max = max(SI),
                          skewness = e1071::skewness(SI),
                          kurtosis = e1071::kurtosis(SI))

# histogram dla Site Index
# ustalić przedziały osi X odpowiadające słupkom
ggplot(data = site_index, aes(x = SI)) +
  geom_histogram(binwidth = 2, color = "black", fill = "lightgrey") +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
  scale_x_continuous(limits = c(5, 45), breaks = c(seq(5, 45, 2))) +
  scale_y_continuous(breaks = c(seq(0, 2500, 200)))+ t

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
                                              IQR = IQR(SI),
                                              q3 = quantile(SI, 0.75),
                                              max = max(SI),
                                              skewness = e1071::skewness(SI),
                                              kurtosis = e1071::kurtosis(SI)) %>%
  dplyr::mutate(kraina = factor(region)) -> srednieSIkrainami

srednieSIkrainami

site_index %>%
  group_by(region) %>%
  summarise(n = paste("n =", length(region))) -> region.labs

# przeanalizować również gęstość!!!
ggplot(data = na.omit(site_index), aes(x = SI)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~region, ncol = 2) +
  geom_text(data = region.labs, aes(x=38, y=300, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) +
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") + t 

ggplot(data = na.omit(site_index), aes(x = region, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
### zamienić zapis naukowy na zwykły!!! - 3 miejsca po przecinku!
kruskal.test(SI ~ region, site_index)
pairwise.wilcox.test(site_index$SI, site_index$region, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ region, data = site_index, method="bh") # test post-hoc

### sprawdzić wartości odstające - jakie siedliska i inne cechy!!!

## ----- klasy_wieku ----------------------------------------------------------------------------------------
site_index %>% group_by(kw) %>% summarize(n = n(),
                                          min = min(SI), 
                                          q1 = quantile(SI, 0.25), 
                                          median = median(SI), 
                                          mean = mean(SI),
                                          sd = sd(SI),
                                          IQR = IQR(SI),
                                          q3 = quantile(SI, 0.75),
                                          max = max(SI),
                                          skewness = e1071::skewness(SI),
                                          kurtosis = e1071::kurtosis(SI))

#propozycja wrzucenia go do pracy
ggplot(data = na.omit(site_index), aes(x = kw, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") + t

# czy jest tak ze mlodsze drzeowstany maja lepsza bonitację?
# czy też wzory są dalej niedskonałe że zawyżają młode?

# test Kruskala-Wallisa
kruskal.test(SI ~ kw, site_index)
pairwise.wilcox.test(site_index$SI, site_index$kw, p.adjust.method = "BH")
# dunnTest(SI ~ kw, data = site_index, method="bh")

### podzielić jeszcze na podklasy wieku!
### sprawdzić czy istnieje korelacja z wiekiem istotna statystycznie
### na medianach współczynnik korelacja spearmana a nie pearsona
## ----- zyznosc ----------------------------------------------------------------------------------------
### kiedy wyróżniamy górskie?
site_index %>% group_by(fertility) %>% summarize(n = n(),
                                                 min = min(SI), 
                                                 q1 = quantile(SI, 0.25), 
                                                 median = median(SI), 
                                                 mean = mean(SI),  
                                                 sd = sd(SI),
                                                 IQR = IQR(SI),
                                                 q3 = quantile(SI, 0.75),
                                                 max = max(SI),
                                                 skewness = e1071::skewness(SI),
                                                 kurtosis = e1071::kurtosis(SI)) 

ggplot(data = na.omit(site_index), aes(x = fertility, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "grupa żyznościowa", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ fertility, site_index)
pairwise.wilcox.test(site_index$SI, site_index$fertility, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ fertility, data = site_index, method="bh") # test post-hoc

## ----- wilgotnosc ----------------------------------------------------------------------------------------
site_index %>% group_by(humidity) %>% summarize(n = n(),
                                                min = min(SI), 
                                                q1 = quantile(SI, 0.25), 
                                                median = median(SI), 
                                                mean = mean(SI),  
                                                sd = sd(SI),
                                                IQR = IQR(SI),
                                                q3 = quantile(SI, 0.75),
                                                max = max(SI),
                                                skewness = e1071::skewness(SI),
                                                kurtosis = e1071::kurtosis(SI))
ggplot(data = na.omit(site_index), aes(x = humidity, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "grupa wilgotnościowa skomasowana", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ humidity, site_index)
pairwise.wilcox.test(site_index$SI, site_index$humidity, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ humidity, data = site_index, method="bh") # test post-hoc

## ----- wilgotnosc-grupami ----------------------------------------------------------------------------------------
site_index %>% group_by(humidity2) %>% summarize(n = n(),
                                                 min = min(SI), 
                                                 q1 = quantile(SI, 0.25), 
                                                 median = median(SI), 
                                                 mean = mean(SI),  
                                                 sd = sd(SI),
                                                 IQR = IQR(SI),
                                                 q3 = quantile(SI, 0.75),
                                                 max = max(SI),
                                                 skewness = e1071::skewness(SI),
                                                 kurtosis = e1071::kurtosis(SI))
ggplot(data = na.omit(site_index), aes(x = humidity2, y = SI)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "grupa wilgotnościowa", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ humidity2, site_index)
pairwise.wilcox.test(site_index$SI, site_index$humidity2, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ humidity2, data = site_index, method="bh") # test post-hoc

## ----- siedlisko ----------------------------------------------------------------------------------------
site_index %>% group_by(habitat) %>% summarize(n = n(),
                                               min = min(SI), 
                                               q1 = quantile(SI, 0.25), 
                                               median = median(SI), 
                                               mean = mean(SI), 
                                               sd = sd(SI),
                                               IQR = IQR(SI),
                                               q3 = quantile(SI, 0.75),
                                               max = max(SI),
                                               skewness = e1071::skewness(SI),
                                               kurtosis = e1071::kurtosis(SI))
ggplot(data = na.omit(site_index), aes(y = SI, x = habitat)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)") + t

ggplot(data = na.omit(site_index), aes(y = SI, x = reorder(habitat, SI, FUN=median))) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)") + t

# test Kruskala-Wallisa
kruskal.test(SI ~ habitat, site_index)
pairwise.wilcox.test(site_index$SI, site_index$habitat, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ habitat, data = site_index, method="bh") # test post-hoc

### podzielić na siedliska nizinne, wyżynne górskie na jednym wykresie - uwypuklić grupy wysokościowe!
### chyba podzielić na 3 "facets" dla poszczególnych grup
### testowe porównania prowadzić również w grupach
### barwienie wyników istotnych statystycznie
### nieużywanie wypluwek z kosnoli do wyświetlania tabel - kable???

## ----- facets ----------------------------------------------------------------------------------------
# wykres w podziale na krainy w zależności od wieku i regionu
ggplot(data = na.omit(site_index), aes(y = SI, x = kw)) +
  geom_boxplot(notch = TRUE) +
  stat_n_text(size = 3) +
  labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") + facet_wrap(~region, ncol = 2) + t

# wykres w podziale na krainy w zależności od regionu i wieku
site_index %>%
  group_by(kw) %>%
  summarise(n = paste("n =", length(kw))) -> kw.labs

ggplot(data = na.omit(site_index), aes(y = SI, x = region)) +
  geom_boxplot(notch = TRUE) +
  geom_text(data = kw.labs, aes(x = 7, y = 50, label = n), colour="black", inherit.aes = FALSE, parse = FALSE) +
  stat_n_text(size = 3) +
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
  facet_wrap(~kw, ncol = 2) + t

# przygotować dla siedlisk w klasach wieku
# współczynnik zmienności dla 100 drzew na hektarze - poszukać danych!!!

## ----- gps ----------------------------------------------------------------------------------------
site_index_gps <- dplyr::left_join(site_index, gps, by = "plot_no") #%>% na.omit()

### backup write/load site_index_gps ---------------------------------------------------------------------------------
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

coordinates(site_index_gps) <- ~ lon + lat #adding sptial relationship
proj4string(site_index_gps) <- "+init=epsg:4326" #adding WGS84 projection

## ----- region-map ----------------------------------------------------------------------------------------
data(Europe, rivers)
# vistula <- subset(rivers, name == "Vistula")
Poland <- subset(Europe, name == "Poland")
plot_map_dots <- function(shape, feature) {
  tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    # tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    tm_shape(shape) + tm_dots(col = feature, size = 0.05, palette = "BrBG", n = 5, auto.palette.mapping = FALSE) +
    tm_style_white(legend.position = c("left", "bottom"))
}

### udział bagiennych i wilgotnych w udziale dla poszczególnych krain

data.frame(table(site_index$region, site_index$habitat)) -> a

plot_map_dots(site_index_gps, "SI")

# mapa ze średnim SI dla krain
# myCols <- adjustcolor(colorRampPalette(brewer.pal(n=8, 'Greens'))(100), .85) #kolorki na przyszłość
plot_map_polys <- function(shape, feature) {
  tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    # tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    tm_shape(shape) + tm_polygons(feature, palette="RdYlGn", n = 5, auto.palette.mapping = FALSE) +
    tm_style_white(legend.position = c("left", "bottom"))
}
krainy.shp <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")
krainy2.shp <- sp::merge(krainy.shp, as.data.frame(srednieSIkrainami), by = "kraina")
plot_map_polys(krainy2.shp, "median")

## ----- inspectorate-map ----------------------------------------------------------------------------------------

site_index %>%
  group_by(kodn) %>%
  summarise(median = median(SI)) -> srednieSInadles

# mapa ze średnim SI dla nadleśnictw
nadles.shp <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "nadles84simple")
nadles2.shp <- sp::merge(nadles.shp, srednieSInadles, by.x="KODN", by.y = "kodn")
plot_map_polys(nadles2.shp, "median")

## ----- inspectorate-map-age ----------------------------------------------------------------------------------------
site_index %>%
  group_by(kw, kodn) %>%
  summarise(median = median(SI)) %>%
  tidyr::spread(key = kw, value = median) %>%
  dplyr::rename("VI+" = 'VI i st') -> srednieSInadlesKW

plot_map_polys2 <- function(shape, feature) {
  tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    # tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    tm_shape(shape) + tm_polygons(feature, palette="RdYlGn", style = "fixed", breaks = c(10, 15, 20, 25, 30, 35, 40, 45), auto.palette.mapping = FALSE) +
    tm_style_white(legend.position = c("left", "bottom"))
} 

nadles3.shp <- sp::merge(nadles.shp, srednieSInadlesKW, by.x="KODN", by.y = "kodn")
kw1 <- plot_map_polys2(nadles3.shp, "I")
kw2 <- plot_map_polys2(nadles3.shp, "II")
kw3 <- plot_map_polys2(nadles3.shp, "III")
kw4 <- plot_map_polys2(nadles3.shp, "IV")
kw5 <- plot_map_polys2(nadles3.shp, "V")
kw6 <- plot_map_polys2(nadles3.shp, "VI+")
tmap_arrange(kw1, kw2, kw3, kw4, kw5, kw6, asp = NA, ncol = 3)

## @knitr

### Rozdział dot statystyk przestrzennych
### statystyka lokalna morana - jeszcze jakieś wskaźniki prestrzenne

### do pobrania z BDLu - uwodnienie siedlisk, zgodność, bonitacja Szymkiewicza

### Rozdział dot. chwappacha - poróWnanie

# dane2 %>%
#   filter(nr_podpow == 60083401)
# 
# dane2 %>%
#   filter(nr_podpow == 92015402)

# jak zmieniała się wysokość na powierzchniach dla poszczególnych drzew i dla średniej H100

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

# linie trendu do siedlisk plus porównać linie trendu na jednym wykresie

# rozstęp większy niż 20 lat - zaingerować
# najniższe SI - sprawdzić gdzie występuje
# histogram i histogram skumulowany - ogólnie dla Polski i w rozbiciu na krainy oraz na TSL
# zależność między wiekiem a SI - o gólnie i w krainach (jak sprawdza się w każdej z nich?)

# różne wieki na powierzchni próbnej w drzewach z pomierzoną wysokością - co z tym fantem?

### solved problems -----
# co z przestojami w kodzie "war" - przestoje maja inny kod "war" = 10 lub 11

### future ideas -----
# porównanie SI między dwoma cyklami? różnice w bonitacjach na tej samej powierzchni
# analiza różnic
# błąd pomiaru wysokości:
# w 5 lat mały przyrost - możliwość dużych błędów przy małych wzrostach