### SI descriptive statistics for PhD research ###

library(tidyverse)
library(tmap)
library(rgdal)
# library(RColorBrewer)
library(feather)
library(EnvStats)
library(sf)
# library(FSA)

## ----- data_loading / wczytanie danych -------------------------------------------------------------------------
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather")) ### comment one
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))
# litography_500k <- read_feather(paste0(getwd(), "/data/WMS/litography_500k.feather"))
# litography_50k <- read_feather(paste0(getwd(), "/data/WMS/litography_50k.feather"))

## function for scaling SI
scale_this <- function(x){
        (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# --- old method / stara metoda ----------------------------------
### Site Index calculations / obliczenia wskaźnika bonitacji ----------------
## ----- values
# values <- tibble(
#         b1 = 1.381,
#         b3 = 32.947,
#         b2 = 4679.9
# )

# ### współczynniki SI w wersji jednolitej dla całej Polski ------
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

### współczynniki Si w rozbiciu na krainy ------
# trees %>%
#         filter(gat == "SO") %>%
#         na.omit(h) %>%
#         dplyr::mutate(b1 = case_when(kraina == 1 ~ 1.281,
#                                      kraina == 2 ~ 1.503,
#                                      kraina == 3 ~ 1.326,
#                                      kraina == 4 ~ 1.408,
#                                      kraina == 5 ~ 1.418,
#                                      kraina == 6 ~ 1.460,
#                                      kraina == 7 ~ 1.317,
#                                      kraina == 8 ~ 1.317),
#                       b3 = case_when(kraina == 1 ~ 32.68,
#                                      kraina == 2 ~ 39.75,
#                                      kraina == 3 ~ 26.72,
#                                      kraina == 4 ~ 22.52,
#                                      kraina == 5 ~ 37.24,
#                                      kraina == 6 ~ 15.14,
#                                      kraina == 7 ~ 29.89,
#                                      kraina == 8 ~ 29.89),
#                       b2 = case_when(kraina == 1 ~ 8813,
#                                      kraina == 2 ~ 2582,
#                                      kraina == 3 ~ 9706,
#                                      kraina == 4 ~ 7520,
#                                      kraina == 5 ~ 2157,
#                                      kraina == 6 ~ 10090,
#                                      kraina == 7 ~ 1420,
#                                      kraina == 8 ~ 1420),
#                       H = h, 
#                       wiek = wiek,
#                       SI_raw = H*((100^b1)*((wiek^b1)*(H-b3+(((H-b3)^2) + 
#                                 (2 * b2 * H) / (wiek ^ b1)) ^ 0.5) + b2)) / ((wiek ^ b1) * 
#                                 ((100 ^ b1) * (H - b3 + (((H - b3) ^ 2) + (2 * b2 * H) /
#                                 (wiek ^ b1)) ^ 0.5) + b2))) %>% 
#         group_by(nr_punktu, nr_podpow) %>%
#         dplyr::summarise(SI = mean(SI_raw)) %>%
#         filter(complete.cases(SI)) -> site_index_raw_2
#
# --- new method / aktualna metoda ----------------------------------
# trees %>%
#         filter(gat == "SO") %>%
#         na.omit(h) %>%
#         group_by(nr_podpow) %>%
#         dplyr::summarise(H = mean(h), wiek = mean(wiek), kraina = first(kraina)) %>%
#         dplyr::mutate(b1 = case_when(kraina == 1 ~ 1.281,
#                                      kraina == 2 ~ 1.503,
#                                      kraina == 3 ~ 1.326,
#                                      kraina == 4 ~ 1.408,
#                                      kraina == 5 ~ 1.418,
#                                      kraina == 6 ~ 1.460,
#                                      kraina == 7 ~ 1.317,
#                                      kraina == 8 ~ 1.317),
#                       b3 = case_when(kraina == 1 ~ 32.68,
#                                      kraina == 2 ~ 39.75,
#                                      kraina == 3 ~ 26.72,
#                                      kraina == 4 ~ 22.52,
#                                      kraina == 5 ~ 37.24,
#                                      kraina == 6 ~ 15.14,
#                                      kraina == 7 ~ 29.89,
#                                      kraina == 8 ~ 29.89),
#                       b2 = case_when(kraina == 1 ~ 8813,
#                                      kraina == 2 ~ 2582,
#                                      kraina == 3 ~ 9706,
#                                      kraina == 4 ~ 7520,
#                                      kraina == 5 ~ 2157,
#                                      kraina == 6 ~ 10090,
#                                      kraina == 7 ~ 1420,
#                                      kraina == 8 ~ 1420),
#                       SI = H*((100^b1)*((wiek^b1)*(H-b3+(((H-b3)^2) + 
#                            (2 * b2 * H) / (wiek ^ b1)) ^ 0.5) + b2)) / ((wiek ^ b1) * 
#                            ((100 ^ b1) * (H - b3 + (((H - b3) ^ 2) + (2 * b2 * H) /
#                            (wiek ^ b1)) ^ 0.5) + b2))) %>% 
#         filter(complete.cases(SI)) -> site_index_raw_3
# 
# trees %>%
#         group_by(nr_podpow) %>%
#         dplyr::left_join(., area, by = "nr_podpow") %>%
#         dplyr::summarise(n_drzew = n(), 
#                          n_gat = n_distinct(gat),
#                          D2 = sum((d13/10)^2, na.rm = TRUE), 
#                          Dq = sqrt(D2/n_drzew),
#                          pow = mean(pow_a/10000),
#                          zag = n_drzew/pow,
#                          SDI = zag * (Dq/25)^(1.605)) %>%
#         dplyr::right_join(., site_index_raw_3, by = "nr_podpow") %>%
#         dplyr::select(-c(D2, Dq, pow, zag, b1, b2, b3, kraina, H, wiek)) -> site_index_raw_4
# 
# sites %>%  
#         dplyr::filter(as.character(sites$gat_pan_pr) == "So") -> sites2
# 
# # main assumption is that on small areas <100m^2 is not enough trees to get proper measurments
# site_index <- dplyr::inner_join(sites2, site_index_raw_4, by = "nr_podpow", suffix = c("", ".y")) %>% 
#         dplyr::left_join(., area, by = "nr_podpow") %>% 
#         # filter(pow_a >= 100, kraina != 8, kraina != 7, grp == "N") # kryterium powirzchni minimalnej usunięto, nie znajdując potwierdzenia różnic SI dla różnych powierzchni
#         filter(kraina != 8, kraina != 7, grp == "N")
# 
# site_index %>% 
#         mutate(z_mean = as.vector(scale(SI, center = TRUE, scale = FALSE)), 
#                z_sd = scale_this(SI),
#                gp = runif(dim(.)[1])) -> site_index

### backup write site_index / kopia zapasowa ------------------------------------------------------------------------------------
# write_feather(site_index, paste0(getwd(), "/data/WISL/site_index.feather"))
# consider for future: using fst to read/write operations?

site_index <- read_feather(paste0(getwd(), "/data/WISL/site_index.feather"))

theme_set(
        theme_bw() +
                theme(legend.position = "top", 
                      axis.text = element_text(size = 11), 
                      axis.title = element_text(size = 12, face = "bold"))
)

### LICZBA MIEJSC PO PRZECINKU!!!

## ----- POLAND / POLSKA ------------------------------------------------------------------------------------------
site_index %>% summarise(n = n_distinct(nr_punktu))
site_index %>% summarise(n = n_distinct(nr_podpow))

site_index %>%  summarize(n = n(),
                          min = min(SI), 
                          q1 = quantile(SI, 0.25), 
                          median = median(SI), 
                          q3 = quantile(SI, 0.75),
                          mean = mean(SI), 
                          sd = sd(SI),
                          max = max(SI),
                          skewness = e1071::skewness(SI),
                          kurtosis = e1071::kurtosis(SI))

shapiro.test(site_index$SI) # max 5000
library(nortest)
lillie.test(site_index$SI)
sf.test(site_index$SI) # max 5000
pearson.test(site_index$SI)
cvm.test(site_index$SI)
ad.test(site_index$SI)
library(moments)
agostino.test(site_index$SI)

# histogram dla Site Index
# ustalić przedziały osi X odpowiadające słupkom
source("geom_marginboxplot.R") # loading user functions
ggplot(data = site_index, aes(x = SI)) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightgrey", boundary = 0, closed = "left") +
        labs(x = "wskaźnik bonitacji (SI)", y = "częstość") +
        scale_x_continuous(limits = c(6, 46), breaks = c(seq(6, 46, 2))) +
        scale_y_continuous(breaks = c(seq(0, 2500, 100))) + 
        geom_marginboxplot(data = site_index, aes(y = 1), sides = "t") +
        geom_vline(data = site_index %>% summarise(mean = mean(SI)), aes(xintercept = mean), linetype = "dashed")

ggsave("img/SI_PL_histogram.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# ggplot(site_index, aes(x = "", y = SI)) + geom_boxplot() + coord_flip() + labs(x = "", y = "wskaźnik bonitacji (SI)") + scale_y_continuous(limits = c(6, 46), breaks = c(seq(6, 46, 2)))
# ggplot(site_index, aes(sample = SI)) + stat_qq()
# ggplot(site_index, aes(SI)) + stat_ecdf(geom = "step")

## ----- regions / krainy ----------------------------------------------------------------------------------------
### podpisać krainy na wykresie
site_index$kraina_nzw <- fct_recode(site_index$kraina, "kraina I" = "1", "kraina II" = "2", "kraina III" = "3", "kraina IV" = "4", 
                                "kraina V" = "5", "kraina VI" = "6", "kraina VII" = "7", "kraina VIII" = "8")

site_index %>% group_by(kraina) %>% summarize(n = n(),
                                              min = min(SI), 
                                              q1 = quantile(SI, 0.25), 
                                              median = median(SI), 
                                              q3 = quantile(SI, 0.75),
                                              mean = mean(SI), 
                                              sd = sd(SI),
                                              max = max(SI),
                                              skewness = e1071::skewness(SI),
                                              kurtosis = e1071::kurtosis(SI),
                                              rozstep = max(SI) - min(SI),
                                              IQR = IQR(SI)) %>%
        dplyr::mutate(kraina = factor(kraina)) %>%
        mutate_at(vars(median, mean), round, 1) -> srednieSIkrainami

srednieSIkrainami

site_index %>%
        group_by(kraina_nzw) %>%
        summarise(n = paste("n =", length(kraina_nzw))) -> kraina.labs

# przeanalizować również gęstość!!!
ggplot(data = site_index, aes(x = SI)) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightgrey", boundary = 0, closed = "left") +
        facet_wrap(~kraina_nzw, ncol = 2) +
        geom_text(data = kraina.labs, aes(x = 40, y = 400, label = n), colour = "black", inherit.aes = TRUE, parse = FALSE) +
        labs(x = "wskaźnik bonitacji (SI)", y = "częstość") + 
        geom_marginboxplot(data = site_index, aes(y = 1), sides = "t") +
        geom_vline(data = site_index %>% group_by(kraina_nzw) %>% summarise(mean = mean(SI)), aes(xintercept = mean), linetype = "dashed")

ggsave("img/SI_krainy_histogram.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

ggplot(data = site_index, aes(x = kraina_nzw, y = SI)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)")

ggsave("img/SI_krainy_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# test Kruskala-Wallisa
### zamienić zapis naukowy na zwykły!!! - 3 miejsca po przecinku!
kruskal.test(SI ~ kraina, site_index)
pairwise.wilcox.test(site_index$SI, site_index$kraina, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ kraina, data = site_index, method = "bh") # test post-hoc

### sprawdzić wartości na różnych powierzchniach
ggplot(data = site_index) +
        geom_jitter(aes(x = tsl, y = SI, color = pow_a)) +
        scale_color_gradient2(midpoint = 250, high = "blue", mid = "white", low = "red")

### wartości odstające - jakie siedliska i inne cechy!!!
ggplot(data = site_index) +
        geom_jitter(aes(x = zyzn, y = SI, color = SI)) +
        scale_color_gradient2(midpoint = 28, high = "blue", mid = "white", low = "red")

### ----- age classes / klasy_wieku -----------------------------------------------------------------------------
site_index %>% group_by(kw) %>% summarize(n = n(),
                                          min = min(SI), 
                                          q1 = quantile(SI, 0.25), 
                                          median = median(SI), 
                                          q3 = quantile(SI, 0.75),
                                          mean = mean(SI), 
                                          sd = sd(SI),
                                          max = max(SI),
                                          skewness = e1071::skewness(SI),
                                          kurtosis = e1071::kurtosis(SI),
                                          rozstep = max(SI) - min(SI),
                                          IQR = IQR(SI))

site_index %>%
        group_by(kw) %>%
        summarise(n = paste("n =", length(kw))) -> kw.labs

# przeanalizować również gęstość!!!
ggplot(data = site_index, aes(x = SI)) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightgrey", boundary = 0, closed = "left") +
        facet_wrap(~kw, ncol = 2) +
        geom_text(data = kw.labs, aes(x = 40, y = 380, label = n), colour = "black", inherit.aes = FALSE, parse = FALSE) +
        labs(x = "wskaźnik bonitacji (SI)", y = "częstość") + 
        geom_marginboxplot(data = site_index, aes(y = 1), sides = "t") +
        geom_vline(data = site_index %>% group_by(kw) %>% summarise(mean = mean(SI)), aes(xintercept = mean), linetype = "dashed")

ggsave("img/SI_kw_histogram.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

#propozycja wrzucenia go do pracy
ggplot(data = site_index, aes(x = kw, y = SI)) +
        geom_boxplot(notch = TRUE) +
        # stat_summary(fun.y=mean, colour="darkred", geom = "point", shape=18, size=1) +
        stat_n_text(size = 3) +
        labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") 

ggsave("img/SI_kw_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# czy jest tak ze mlodsze drzeowstany maja lepsza bonitację?
# czy też wzory są dalej niedskonałe że zawyżają młode?

# test Kruskala-Wallisa
kruskal.test(SI ~ kw, site_index)
pairwise.wilcox.test(site_index$SI, site_index$kw, p.adjust.method = "BH")
# dunnTest(SI ~ kw, data = site_index, method = "bh")

### podzielić jeszcze na podklasy wieku!
### sprawdzić czy istnieje korelacja z wiekiem istotna statystycznie
### na medianach współczynnik korelacja spearmana a nie pearsona
## ----- fertility / zyznosc ----------------------------------------------------------------------------------------
site_index %>% group_by(zyzn2) %>% summarize(n = n(),
                                            min = min(SI), 
                                            q1 = quantile(SI, 0.25), 
                                            median = median(SI), 
                                            q3 = quantile(SI, 0.75),
                                            mean = mean(SI), 
                                            sd = sd(SI),
                                            max = max(SI),
                                            skewness = e1071::skewness(SI),
                                            kurtosis = e1071::kurtosis(SI),
                                            rozstep = max(SI) - min(SI),
                                            IQR = IQR(SI))

ggplot(data = site_index, aes(x = zyzn, y = SI)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "grupa żyznościowa", y = "wskaźnik bonitacji (SI)")  + 
        scale_x_discrete(labels = c("Bory", "Bory Mieszane", "Lasy Mieszane", "Lasy")) 

ggsave("img/SI_zyzn_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# test Kruskala-Wallisa
kruskal.test(SI ~ zyzn2, site_index)
pairwise.wilcox.test(site_index$SI, site_index$zyzn2, p.adjust.method = "BH")#$p.value %>% kableExtra::kable() %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) # test post-hoc
# dunnTest(SI ~ fertility, data = site_index, method="bh") # test post-hoc

## ----- humidity / wilgotnosc ----------------------------------------------------------------------------------------
site_index %>% filter(wilg1 != "z") -> site_index_wilg1

site_index_wilg1 %>% group_by(wilg) %>% summarize(n = n(),
                                             min = min(SI), 
                                             q1 = quantile(SI, 0.25), 
                                             median = median(SI), 
                                             q3 = quantile(SI, 0.75),
                                             mean = mean(SI), 
                                             sd = sd(SI),
                                             max = max(SI),
                                             skewness = e1071::skewness(SI),
                                             kurtosis = e1071::kurtosis(SI),
                                             rozstep = max(SI) - min(SI),
                                             IQR = IQR(SI))

ggplot(data = site_index_wilg1, aes(x = wilg1, y = SI)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "grupa wilgotnościowa", y = "wskaźnik bonitacji (SI)") + 
        scale_x_discrete(labels = c("suche", "swieże", "wilgotne", "bagienne")) 

ggsave("img/SI_wilg_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# test Kruskala-Wallisa
kruskal.test(SI ~ wilg, site_index_wilg1)
pairwise.wilcox.test(site_index_wilg1$SI, site_index_wilg1$wilg, p.adjust.method = "BH")#$p.value %>% kableExtra::kable() %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) # test post-hoc
# dunnTest(SI ~ humidity, data = site_index, method="bh") # test post-hoc

# ## ----- wilgotnosc-grupami ----------------------------------------------------------------------------------------
# site_index %>% group_by(wilg2) %>% summarize(n = n(),
#                                              min = min(SI), 
#                                              q1 = quantile(SI, 0.25), 
#                                              median = median(SI), 
#                                              q3 = quantile(SI, 0.75),
#                                              mean = mean(SI), 
#                                              sd = sd(SI),
#                                              max = max(SI),
#                                              skewness = e1071::skewness(SI),
#                                              kurtosis = e1071::kurtosis(SI))
# 
# ggplot(data = site_index, aes(x = wilg2, y = SI)) +
#         geom_boxplot(notch = TRUE) +
#         stat_n_text(size = 3) +
#         labs(x = "grupa wilgotnościowa", y = "wskaźnik bonitacji (SI)")
# 
# ggsave("img/SI_wilg2_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
# dev.off()
# 
# # test Kruskala-Wallisa
# kruskal.test(SI ~ wilg2, site_index)
# pairwise.wilcox.test(site_index$SI, site_index$wilg2, p.adjust.method = "BH") # test post-hoc
# # dunnTest(SI ~ humidity2, data = site_index, method="bh") # test post-hoc

## ----- habitat / siedlisko ----------------------------------------------------------------------------------------
'%ni%' <- Negate('%in%')
site_index %>% filter(tsl %ni% c("Ol", "OlJ", "Lł")) -> site_index_tsl2

site_index_tsl2 %>% group_by(tsl) %>% summarize(n = n(),
                                                min = min(SI), 
                                                q1 = quantile(SI, 0.25), 
                                                median = median(SI), 
                                                q3 = quantile(SI, 0.75),
                                                mean = mean(SI), 
                                                sd = sd(SI),
                                                max = max(SI),
                                                skewness = e1071::skewness(SI),
                                                kurtosis = e1071::kurtosis(SI),
                                                rozstep = max(SI) - min(SI),
                                                IQR = IQR(SI))

ggplot(data = site_index_tsl2, aes(y = SI, x = tsl)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        coord_flip() +
        labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)") 

ggsave("img/SI_tsl_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

ggplot(data = site_index_tsl2, aes(y = SI, x = reorder(tsl, SI, FUN = median))) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        coord_flip() +
        labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)") 

# test Kruskala-Wallisa
kruskal.test(SI ~ tsl, site_index_tsl2)
pairwise.wilcox.test(site_index_tsl2$SI, site_index_tsl2$tsl, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ tsl, data = site_index_tsl2, method="bh") # test post-hoc

## ----- facets ----------------------------------------------------------------------------------------
# wykres w podziale na krainy w zależności od wieku i regionu
ggplot(data = na.omit(site_index), aes(y = SI, x = kw)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "klasa wieku (kw)", y = "wskaźnik bonitacji (SI)") + facet_wrap(~kraina, ncol = 2) 

# wykres w podziale na krainy w zależności od regionu i wieku
site_index %>%
        group_by(kw) %>%
        summarise(n = paste("n =", length(kw))) -> kw.labs

ggplot(data = na.omit(site_index), aes(y = SI, x = kraina)) +
        geom_boxplot(notch = TRUE) +
        geom_text(data = kw.labs, aes(x = 7, y = 50, label = n), colour="black", inherit.aes = FALSE, parse = FALSE) +
        stat_n_text(size = 3) +
        labs(x = "kraina przyrodniczo-leśna (kpl)", y = "wskaźnik bonitacji (SI)") +
        facet_wrap(~kw, ncol = 2) 

# przygotować dla siedlisk w klasach wieku
# współczynnik zmienności dla 100 drzew na hektarze - poszukać danych!!!

## ----- ownership / własność -----------------------------------------------------------------------------------------
site_index %>% group_by(wlasc) %>% summarize(n = n(),
                                             min = min(SI), 
                                             q1 = quantile(SI, 0.25), 
                                             median = median(SI), 
                                             q3 = quantile(SI, 0.75),
                                             mean = mean(SI), 
                                             sd = sd(SI),
                                             max = max(SI),
                                             skewness = e1071::skewness(SI),
                                             kurtosis = e1071::kurtosis(SI),
                                             rozstep = max(SI) - min(SI),
                                             IQR = IQR(SI))

ggplot(data = site_index, aes(x = wlasc, y = SI)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "własność", y = "wskaźnik bonitacji (SI)") + 
        scale_x_discrete(labels = c("Lasy Państwowe", "parki narodowe", "lasy prywatne", "inne Skarbu Państwa"))

ggsave("img/SI_wlasc_boxplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# test Kruskala-Wallisa
kruskal.test(SI ~ wlasc, site_index)
pairwise.wilcox.test(site_index$SI, site_index$wlasc, p.adjust.method = "BH")#$p.value %>% kableExtra::kable() %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) # test post-hoc # test post-hoc
# dunnTest(SI ~ humidity2, data = site_index, method="bh") # test post-hoc

site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
        group_by(wlasc) %>% summarize(n = n(),
                                      min = min(SI), 
                                      q1 = quantile(SI, 0.25), 
                                      median = median(SI), 
                                      q3 = quantile(SI, 0.75),
                                      mean = mean(SI), 
                                      sd = sd(SI),
                                      max = max(SI),
                                      skewness = e1071::skewness(SI),
                                      kurtosis = e1071::kurtosis(SI))       

### PGL LP vs Inne
site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
        ggplot(data = ., aes(x = wlasc, y = SI)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "własność", y = "wskaźnik bonitacji (SI)")

# test Kruskala-Wallisa
site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
kruskal.test(SI ~ wlasc, .)
# site_index %>% 
#         mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
# pairwise.wilcox.test(.$SI, .$wlasc, p.adjust.method = "BH") # test post-hoc
# dunnTest(SI ~ humidity2, data = site_index, method="bh") # test post-hoc

### własność vs siedliska

site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
        ggplot(data = ., aes(y = SI, x = tsl, fill = wlasc)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        coord_flip() +
        labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)")

site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
        ggplot(data = ., aes(y = SI, x = zyzn, fill = wlasc)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "grupa żyznościowa", y = "wskaźnik bonitacji (SI)") +
        scale_fill_discrete(name = "")

site_index %>% 
        mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
        ggplot(data = ., aes(y = SI, x = wilg1, fill = wlasc)) +
        geom_boxplot(notch = TRUE) +
        stat_n_text(size = 3) +
        labs(x = "grupa wilgotnościowa", y = "wskaźnik bonitacji (SI)") +
        scale_fill_discrete(name = "")

# site_index %>% 
#         mutate(wlasc = fct_lump(wlasc, n = 1, other_level = "Inne")) %>%
#         ggplot(data = ., aes(y = SI, x = wilg2, fill = wlasc)) +
#         geom_boxplot(notch = TRUE) +
#         stat_n_text(size = 3) +
#         labs(x = "siedliskowy typ lasu", y = "wskaźnik bonitacji (SI)")

### TSL a krainy



tsl_statistics <- function(f = "Bśw", var = "SI") {
        site_index %>% filter(tsl == f) %>% group_by(kraina_nzw, kw) %>% summarise(m = mean(SI)) -> a
        site_index %>% filter(tsl == f) %>% group_by(kraina_nzw, kw) %>% summarise(sd = sd(SI)) -> b
        left_join(a, b, by = c("kraina_nzw", "kw")) %>% mutate_if(is.numeric, round, 1) %>% 
                unite("m±sd", c(m, sd), sep = " ± ") %>% spread(kw, `m±sd`)
}

tsl_statistics(f = "Bśw")
tsl_statistics(f = "BMśw")
tsl_statistics(f = "LMśw")

### mediana i odchylenie ćwiartkowe
# site_index %>% filter(tsl == "Bśw") %>% group_by(kraina, kw) %>% dplyr::summarise(mediana = median(SI)) %>% spread(., kw, mediana)
# site_index %>% filter(tsl == "Bśw") %>% group_by(kraina, kw) %>% dplyr::summarise(odch4 = IQR(SI)/2) %>% spread(., kw, odch4)

## ----- gps ----------------------------------------------------------------------------------------
site_index_gps <- dplyr::left_join(site_index, gps, by = "nr_punktu") #%>% na.omit()

### backup write/load site_index_gps
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

site_index_gps <- st_as_sf(site_index_gps, coords = c('dlugosc', 'szerokosc'), crs = "+init=epsg:4326")

## ----- region-map / mapa dla krain ----------------------------------------------------------------------------
Poland.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "Region") %>% filter(COUNTRY == "Poland") 
krainy.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")
# DOL.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp\\DOL81_3", layer = "DOL_81_3_smooth_3km")
krainySI.shp <- merge(krainy.shp, srednieSIkrainami, by = "kraina")

### udział bagiennych i wilgotnych w udziale dla poszczególnych krain
# data.frame(table(site_index$kraina, site_index$tsl)) -> a
# plot_map_dots(site_index_gps, "SI")

# mapa ze średnim SI dla krain
# myCols <- adjustcolor(colorRampPalette(brewer.pal(n=8, 'Greens'))(100), .85) #kolorki na przyszłość
SI_srednia <- plot_map_polys(krainySI.shp, "mean", t = "średnia [m]")
tmap_save(SI_srednia, filename = "mapy/map_krainy_mean.png", dpi = 600, width = 12, height = 11, units = "cm")

SI_mediana <- plot_map_polys(krainySI.shp, "median", t = "mediana [m]")
tmap_save(SI_mediana, filename = "mapy/map_krainy_median.png", dpi = 600, width = 12, height = 11, units = "cm")

## ----- inspectorate-map / mapa dla nadleśnictw --------------------------------------------------------------------
# 
# site_index %>%
#         group_by(kodn) %>%
#         summarise(median = median(SI)) -> srednieSInadles
# 
# # mapa ze średnim SI dla nadleśnictw
# nadles.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "nadles84simple")
# nadles2.shp <- merge(nadles.shp, srednieSInadles, by.x = "KODN", by.y = "kodn")
# plot_map_polys(nadles2.shp, "median")

## ----- inspectorate-map-age ----------------------------------------------------------------------------------------
site_index %>%
        group_by(kw, kodn) %>%
        summarise(median = median(SI)) %>%
        tidyr::spread(key = kw, value = median) %>%
        dplyr::rename("VI" = 'VI i st') -> srednieSInadlesKW

nadles3.shp <- merge(nadles.shp, srednieSInadlesKW, by.x="KODN", by.y = "kodn")
kw1 <- plot_map_polys(nadles3.shp, "I")
kw2 <- plot_map_polys(nadles3.shp, "II")
kw3 <- plot_map_polys(nadles3.shp, "III")
kw4 <- plot_map_polys(nadles3.shp, "IV")
kw5 <- plot_map_polys(nadles3.shp, "V")
kw6 <- plot_map_polys(nadles3.shp, "VI")
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