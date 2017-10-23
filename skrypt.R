rm(list=ls())
setwd("D:\\Praca\\Badania\\Doktorat\\")

library(dplyr)
library(dbplyr)
library(tidyr)
library(plotly)
library(data.table)
library(rgdal)
library(sp)
library(FSA)
library(RColorBrewer)
library(EnvStats)

dane <- read.csv2("dane.txt", dec = ".", stringsAsFactors = TRUE, sep="\t")

str(dane)
colnames(dane) <- tolower(colnames(dane))

dane %>% 
  dplyr::select(id, nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, h, d13, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
                b_pion_pow_pr, tsl, ukszt_ter, okr_tsl, stan_siedl, pow_a) %>% 
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO", gat == "SO", war == 1) %>%
  dplyr::mutate(nadles = paste0(rdlp, "-", nadl)) -> dane2 # tworzenie klas wieku
summary(dane2)
str(dane2)
hist(dane2$h)
qqnorm(dane2$h)
boxplot(dane2$h, outline = FALSE)
plot(ecdf(dane2$h))
summary(dane2$h, na.rm = TRUE)

dane2 %>%
  group_by(nr_podpow) %>%
  filter(!is.na(h)) %>%
  summarise(n = n_distinct(h)) -> liczba_h

liczba_h %>%
  group_by(n) %>%
  summarise(z = n_distinct(nr_podpow)) -> liczba_pow

plot(liczba_pow)


###########
### OLD ###
###########
o <- 0.7445
r <- -0.4531

dane2 %>%
  group_by(nr_podpow) %>%
  filter(!is.na(d13)) %>%
  summarise(kraina = mean(kraina),
            tsl = mean(tsl),
            rdlp = first(rdlp),
            kodn = first(nadles),
            n_d = n_distinct(d13), # liczba pierśnic na powierzchni próbnej
            pow = mean(pow_a), # wielkość powierzchni próbnej - co z małymi powierzchniami?
            zageszczenie = n_d/(pow/10000),
            wskaznik = ceiling(pow/100), # liczba arów powierzchni = liczba drzew do SI
            D100 = sqrt(mean((d13[order(-d13)[1:wskaznik]])^2, na.rm = TRUE))/10, # średnia kwadratowa najgrubszych pierśnic
            Dg = sqrt(mean(d13^2))/10, # średnia kwadratowa wszystkich pierśnic na powierzchni
            n_h = n_distinct(h), # liczba pomierzonych wysokości
            Hg = weighted.mean(h, d13, na.rm = TRUE), # średnia wysokość ważona pierśnicą
            b = o*(Hg^r), 
            a = (Dg/(sqrt(Hg-1.3)))-b*Dg, 
            H100 = (D100/(a+b*D100))^2+1.3, # wysokość 100 najgruszych drzew na ha
            wiek = mean(wiek), # wiek powierzchni
            A = (wiek/(22.222222 + 0.777778 * wiek))^2, # standaryzowana funkcja wzrostu wysokości
            SI = H100/A) %>% 
  dplyr::mutate(kw = cut(wiek, breaks=seq(0, 260, by=20))) %>%
  filter(pow > 199) %>%
  arrange(desc(SI)) -> wynik

levels(wynik$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")
wynik$kraina <- factor(wynik$kraina)

####################
### Wersja Sochy ###
####################

o <- 0.7445
r <- -0.4531

b1 <- 1.381
b3 <- 32.947
b2 <- 4679.9

dane2 %>%
  group_by(nr_podpow) %>%
  filter(!is.na(d13)) %>%
  summarise(nr_punktu = first(nr_punktu),
            kraina = mean(kraina),
            tsl = first(tsl),
            ukszt_ter = first(ukszt_ter),
            rdlp = first(rdlp),
            kodn = first(nadles),
            n_d = n_distinct(d13), # liczba pierśnic na powierzchni próbnej
            pow = mean(pow_a), # wielkość powierzchni próbnej - co z małymi powierzchniami?
            zageszczenie = n_d/(pow/10000),
            wskaznik = ceiling(pow/100), # liczba arów powierzchni = liczba drzew do SI
            D100 = sqrt(mean((d13[order(-d13)[1:wskaznik]])^2, na.rm = TRUE))/10, # średnia kwadratowa najgrubszych pierśnic
            Dg = sqrt(mean(d13^2))/10, # średnia kwadratowa wszystkich pierśnic na powierzchni
            n_h = n_distinct(h), # liczba pomierzonych wysokości
            Hg = weighted.mean(h, d13, na.rm = TRUE), # średnia wysokość ważona pierśnicą
            b = o*(Hg^r), 
            a = (Dg/(sqrt(Hg-1.3)))-b*Dg, 
            H = (D100/(a+b*D100))^2+1.3, # wysokość 100 najgruszych drzew na ha
            wiek = mean(wiek), # wiek powierzchni
            SI=H*((100^b1)*((wiek^b1)*(H-b3+(((H-b3)^2)+(2*b2*H)/(wiek^b1))^0.5)+b2))/((wiek^b1)*((100^b1)*(H-b3+(((H-b3)^2)+(2*b2*H)/(wiek^b1))^0.5)+b2))) %>% 
  dplyr::mutate(kw = cut(wiek, breaks=seq(0, 260, by=20))) %>%
  filter(pow > 199) %>%
  arrange(desc(SI)) -> wynik2

levels(wynik2$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")
wynik2$kraina <- factor(wynik2$kraina)
# backup wyników
# write.table(wynik2, "result.txt", sep="\t", row.names=FALSE)

### połączenie ze współrzędnymi GPS

colnames(gps.coord) <- tolower(colnames(gps.coord))
wynikGPS <- dplyr::left_join(wynik2, gps.coord, by = "nr_punktu")
wynikGPS %>%
  select(point_no = nr_punktu, subpoint_no = nr_podpow, lat = szerokosc, long = dlugosc, SI, tsl, ukszt_ter) -> wynikGPS
  
write.table(wynikGPS, "resultGPS.txt", sep="\t", row.names=FALSE)

#########################
### WYNIKI DLA POLSKI ###
#########################

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

#########################
### WYNIKI DLA KRAIN  ###
#########################

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

################################
### WYNIKI DLA NADLEŚNICTW   ###
################################

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

########################
### WYNIKI DLA TSL   ###
########################

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

# co z przestojami w kodzie "war"

# sprawdzić jak ma się zależność od wzoru Sochy - celowość tworzenia modeli
# uzyć krainy gdzie Socha badał w południowej Polsce żeby to sprawdzić i porównać

# porównanie SI między dwoma cyklami? różnice w bonitacjach na tej samej powierzchni
# analiza różnic
# błąd pomiaru wysokości:
# w 5 lat mały przyrost - możliwość dużych błędów przy małych wzrostach

# prezentacja na semianrium - użyć starej prezentacji: zmodyfikować wykres prezentujący SI
#  - historia SI
#  - metodyka - wzór dynamiczny i statyczny
#  - pierwsze wyniki
#      - dwie grupy: wielkości bonitacji w różnych podziałach
#      - badanie związku z różnymi elementami środowiska

### SOSNA A
b1 <- 1.46138930116412 #B
b2 <- 10.2415777396998 #C
b3 <- -4.52984673991139 #D
### SOSNA B
# b1 <- 1.18309592261132 #B
# b2 <- 13.9780231187729 #C
# b3 <- -3024.12789944235 #D
wiek <- 25 #E
wys <- 12 #F

SoA <- wys*(100^b1*(wiek^b1*(-b3+wys+sqrt((-b3+wys)^2+2*wys*exp(b2)/(wiek^b1)))+exp(b2))/(wiek^b1*(100^b1*(-b3+wys+sqrt((-b3+wys)^2+2*wys*exp(b2)/(wiek^b1)))+exp(b2))))
