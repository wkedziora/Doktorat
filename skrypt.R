# rm(list=ls())
library(tidyverse)
library(tmap)
# library(rgdal)
library(sp)
# library(FSA)
library(RColorBrewer)
# library(EnvStats)

# data loading -----

# maybe feather? df <- read_feather(paste0(getwd(), "/data/sites"))

sites_loading <- cols(plot_no = col_integer(), # column classes for proper data loading  
                      subplot_no = col_integer(),
                      cycle_year = col_factor(levels = NULL),
                      region = col_factor(levels = NULL),
                      plot_species = col_factor(levels = NULL),
                      plot_age = col_factor(levels = NULL),
                      vertical = col_factor(levels = NULL),
                      habitat = col_factor(levels = NULL),
                      habitat_style = col_factor(levels = NULL),
                      habitat_status = col_factor(levels = NULL)
)
sites <- read_tsv("data/sites.txt", col_types = sites_loading)
area <- read_tsv("data/area.txt")
gps_coord <- read_tsv("data/gps_coord.txt")

litography_50k_loading <- cols(tsl = col_factor(levels = NULL), 
                                ukszt_ter = col_factor(levels = NULL)) 
litography_50k <- read_tsv("data/litography_50k.txt", col_types = litography_50k_loading)

litography_500k_loading <- cols(tsl = col_factor(levels = NULL), 
                      ukszt_ter = col_factor(levels = NULL)) 
litography_500k <- read_tsv("data/litography_500k.txt", col_types = litography_500k_loading)

trees_loading <- cols(species = col_factor(levels = NULL)) 
trees <- read_tsv("data/trees.txt", col_types = trees_loading)

# data wrangling -----
ggplot(trees, aes(h)) + geom_freqpoly(binwidth = 1)
ggplot(trees, aes(x = "", y = h)) + geom_boxplot() + coord_flip()
ggplot(trees, aes(sample = h)) + stat_qq()
ggplot(trees, aes(h)) + stat_ecdf(geom = "step")
summary(trees$h, na.rm = TRUE)

# no of sample plots with different no of h measured
trees %>%
  filter(!is.na(h)) %>%
  group_by(subplot_no) %>%
  summarise(n = n_distinct(h)) %>%
  group_by(n) %>%
  summarise(z = n_distinct(subplot_no)) %>%
  ggplot(aes(n, z)) +
  geom_bar(stat = "identity")

trees %>%
  group_by(subplot_no) %>%
  filter(!is.na(dbh), species == "SO") %>%
  left_join(., area, by = "subplot_no") %>%
  filter(!is.na(h)) %>%
  group_by(subplot_no) %>%
  summarise(n = n_distinct(h)) %>%
  group_by(n) %>%
  summarise(z = n_distinct(subplot_no)) %>%
  ggplot(aes(n, z)) +
  geom_bar(stat = "identity")

### Site Index calculations ----------------

values <- tibble(
  b1 = 1.381,
  b3 = 32.947,
  b2 = 4679.9
)

# main assumption is that I only count in Pine trees
trees %>%
  group_by(plot_no, subplot_no) %>%
  filter(species == "SO") %>%
  summarise(
    # n_d = add_count(d13), # liczba pierśnic na powierzchni próbnej
    # n_h = n_distinct(h), # liczba pomierzonych wysokości
    H = mean(h, na.rm = TRUE), # średnia wysokość ważona pierśnicą
    wiek = mean(age), # wiek powierzchni
    SI = H * ((100 ^ values$b1) * ((wiek ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + 
         (2 * values$b2 * H) / (wiek ^ values$b1)) ^ 0.5) + values$b2)) / ((wiek ^ values$b1) * 
         ((100 ^ values$b1) * (H - values$b3 + (((H - values$b3) ^ 2) + (2 * values$b2 * H) /
         (wiek ^ values$b1)) ^ 0.5) + values$b2))) %>% 
  filter(complete.cases(SI)) %>% # muszę w jakiś sposób usunąć wpisy gdzie SO nie jest panująca
  dplyr::mutate(kw = cut(wiek, breaks=seq(0, 260, by=20))) %>%
  arrange(desc(SI)) -> site_index_raw

# main assumption is that on small areas <200m^2 is not enough trees to get proper measurments
site_index <- dplyr::inner_join(sites, site_index_raw, by = "subplot_no", suffix = c("", ".y")) %>% 
  dplyr::left_join(., area, by = "subplot_no") %>% 
  filter(area >= 200)
 
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

site_index %>% mutate(z_mean = as.vector(scale(SI, center = TRUE, scale = FALSE)), z_sd = scale_this(SI)) -> site_index

levels(site_index$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

ggplot(site_index, aes(SI)) + geom_freqpoly(binwidth = 1)
ggplot(site_index, aes(x = "", y = SI)) + geom_boxplot()
ggplot(site_index, aes(sample = SI)) + stat_qq()
ggplot(site_index, aes(SI)) + stat_ecdf(geom = "step")
summary(site_index$SI, na.rm = TRUE)

ggplot(data = site_index, aes(x = kw, y = SI)) + geom_boxplot()

site_index_gps <- dplyr::left_join(site_index, gps_coord, by = "plot_no") #%>% na.omit()

coordinates(site_index_gps) <- ~ lon + lat #adding sptial relationship
proj4string(site_index_gps) <- "+init=epsg:4326" #adding WGS84 projection

# site index map plotting -----
data(Europe, rivers)
vistula <- subset(rivers, name == "Vistula")
tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  tm_shape(site_index_gps) + tm_dots(col = "SI", size = 0.05, palette = "PiYG", n = 5, auto.palette.mapping = FALSE) +
  tm_style_white(legend.position = c("left", "bottom"))
#


### spatstat -------------------------------------------------------------------------------------------------------
library(spatstat)
library(spdep)
library(maptools)

test2 <- dnearneigh(site_index_area_gps, 0, 1, longlat = TRUE)

test4 <- localmoran(site_index_area_gps$SI, nb2listw(test2, zero.policy = TRUE), na.action = na.omit)

site_index_area_gps@data <- data.frame(site_index_area_gps@data, as.data.frame(test4)) 

tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  tm_shape(site_index_area_gps) + tm_dots(col = "Ii", size = 0.05, palette = "PiYG", n = 5, auto.palette.mapping = FALSE) +
  tm_style_white(legend.position = c("left", "bottom"))














  wynikGPS %>%
  select(point_no = nr_punktu, subpoint_no = nr_podpow, lat = szerokosc, long = dlugosc, SI, tsl, ukszt_ter) -> wynikGPS
  
write.table(wynikGPS, "resultGPS.txt", sep="\t", row.names=FALSE)

# results for Poland -----

wynik2 %>% summarise(srednia = mean(SI), 
                    mediana = median(SI),
                    IQR = IQR(SI)) -> srednieSI

# histogram dla Site Index
ggplot(data = wynik, aes(x = SI)) +
  geom_histogram(binwidth = 4) + 
  labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 60))

# punkty dla Site Index
ggplot(data = wynik2, aes(x = wiek, y = SI)) +
  geom_point() + 
  labs(x = "wiek", y = "wskaźnik bonitacji (SI)") +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_continuous(limits = c(0, 60))

# histogram skumulowany
h <- hist(wynik2$SI, breaks = seq(6, 58, by=4))
h$counts <- cumsum(h$counts)
plot(h)

# wynik w zależności od wieku
# xlabs <- paste(levels(wynik$kw),"\n(N=",table(wynik$kw),")",sep="") # stara funkcja dopisująca etykiety
ggplot(data = na.omit(wynik2), aes(x = kw, y = SI)) + 
  geom_boxplot(notch = TRUE) + 
  stat_n_text() +
  theme_bw() +
  labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

# test Kruskala-Wallisa
kruskal.test(SI ~ kw, wynik2)

pairwise.wilcox.test(wynik2$SI, wynik2$kw, p.adjust.method = "BH")
dunnTest(SI ~ kw, data = wynik, method="bh")

# results for regions -----

wynik2 %>% 
  group_by(kraina) %>%
  summarise(srednia = mean(SI),
            mediana = median(SI),
            IQR = IQR(SI)) -> srednieSIkrainami

# histogram w podziale na krainy dla Site Index
wynik2 %>%
  group_by(kraina) %>%
  summarise(n = paste("n =", length(kraina))) -> kraina.labs

ggplot(data = wynik, aes(x = SI)) +
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
# xlabs <- paste(sort(unique(wynik$kraina)),"\n(N=",table(wynik$kraina),")",sep="") # stara funkcja dopisująca etykiety
ggplot(data = wynik, aes(x = as.factor(kraina), y = SI)) + 
  geom_boxplot(notch = TRUE) + 
  stat_n_text() +
  theme_bw() + 
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

# test Kruskala-Wallisa
kruskal.test(SI ~ kraina, wynik)
pairwise.wilcox.test(wynik$SI, wynik$kraina, p.adjust.method = "BH") # test post-hoc
dunnTest(SI ~ kraina, data = wynik, method="bh") # test post-hoc

# wykres w podziale na krainy w zależności od wieku
wynik %>%
  group_by(kw) %>%
  summarise(n = paste("n =", length(kw))) -> kw.labs

ggplot(data = wynik, aes(x = kraina, y = SI, group = kraina)) + 
  geom_boxplot(notch = TRUE, varwidth = TRUE)  + 
  geom_text(data = kw.labs, aes(x=7, y=60, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) +
  # stat_n_text() + 
  facet_wrap(~kw) + 
  theme_bw() + 
  labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
  scale_y_continuous(limits = c(0, 60))

## results for Forest Inspectorates -----

wynik %>% 
  group_by(kodn) %>%
  summarise(mediana = median(SI)) -> srednieSInadles

wynik %>% 
  group_by(kw, kodn) %>%
  summarise(mediana = median(SI)) %>%
  tidyr::spread(key = kw, value = mediana) -> srednieSInadlesKW

wynik2 %>% 
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

wynik %>% 
  group_by(tsl) %>%
  summarise(srednia = mean(SI),
            mediana = median(SI),
            IQR = IQR(SI)) -> srednieSItsl

# histogram w podziale na RDLP dla Site Index
SI <- ggplot(data = wynik, aes(x = SI)) 
SI  + geom_histogram() + facet_wrap(~tsl)

# wykres w podziale na tsl w zależności od wieku
ggplot(data = subset(wynik, !is.na(tsl)), aes(x = wiek, y = SI)) + geom_point() + facet_wrap(~tsl)

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

