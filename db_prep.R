#loading libraries
# library(dplyr)
# library(dbplyr)
# library(tidyr)
# library(plotly)
# library(data.table)
library(odbc)
library(DBI)
library(tidyverse)

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
dbListTables(con) #listing all tables available

# data loading -----
sites_raw <- dbReadTable(con, "ADRES_POW")
trees_raw <- dbReadTable(con, "DRZEWA_OD_7")
plot_a_raw <- dbReadTable(con, "POW_A_B")
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU")

# data wrangling -----
# I am extracting area of sample plot for later usage
plot_a_raw %>%
  as_tibble(.) %>%
  select(nr_cyklu = NR_CYKLU,
         nr_podpow = NR_PODPOW,
         pow = POW_A) %>%
  filter(nr_cyklu == 2) -> plot_a

# I need double number for later use of Spatial Data 
gps_coord_raw %>%
  as_tibble(.) %>%
  type_convert(.) %>%
  select(nr_punktu = NR_PUNKTU,
         rok_w_cyklu = ROK_W_CYKLU,
         lat = SZEROKOSC,
         lon = DLUGOSC) -> gps_coord

# I am loading some independent data
sites_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>%
  select(nr_punktu, nr_cyklu, nr_podpow, rok_w_cyklu, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
         b_pion_pow_pr, tsl, okr_tsl, stan_siedl) %>%
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO") %>%
  type_convert(., col_types = cols_only(gat_pan_pr = col_factor(levels = NULL))) -> sites

# loading tree data

trees_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>%
  select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
  dplyr::filter(nr_cyklu == 2, war == 1) %>%
  type_convert(., col_types = cols_only(nr_punktu = col_integer()))-> trees

#sample for WMS_GetFeatureInfo.R testing
write.table(sample_n(gps_coord, 100), "gps.coord.sample.txt", sep="\t", row.names=FALSE)

# #przygotowanie połączenia
# kwerenda1 %>% dplyr::filter(nr_cyklu == 2) -> kwerenda1a
# 
# kwerenda2 %>% dplyr::filter(nr_cyklu == 2) %>% select(-nr_cyklu) -> kwerenda2a
# 
# dane <- dplyr::full_join(kwerenda1a, kwerenda2a, by = "nr_podpow")
# 
# Dodanie wiodących zer do późniejszych kodów ADR_FOR
# library(stringr)
# dane$rdlp <- str_pad(dane$rdlp, 2, pad = "0")
# dane$nadl <- str_pad(dane$nadl, 2, pad = "0")
# write.table(dane, "dane.txt", sep="\t", row.names=FALSE)

# dane <- data.table::fread("drzewa_od_7.csv")
# dane <- data.table::fread("dane.txt", dec = ".", stringsAsFactors = TRUE)
dane <- read.csv2("dane.txt", dec = ".", stringsAsFactors = TRUE, sep="\t")

str(dane)
colnames(dane) <- tolower(colnames(dane))

dane %>% 
  dplyr::select(id, nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, h, d13, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
                b_pion_pow_pr, tsl, okr_tsl, stan_siedl, pow_a) %>% 
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO", gat == "SO", war == 1) %>%
  dplyr::mutate(nadles = paste0(rdlp, "-", nadl)) -> dane2 # tworzenie klas wieku
