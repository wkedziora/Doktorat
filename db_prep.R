#############################################
### Data import and tidy for PhD research ###
#############################################

library(odbc)
library(DBI)
library(tidyverse)

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
sites_raw <- dbReadTable(con, "ADRES_POW") #site description
trees_raw <- dbReadTable(con, "DRZEWA_OD_7") #tree measurments
plot_a_raw <- dbReadTable(con, "POW_A_B") #plot data
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates

# data wrangling -----
# I am querying for area of sample plot needed later
plot_a_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  select(nr_podpow = NR_PODPOW, # selecting only interesitng colmuns
         pow = POW_A) -> plot_a # creating a new tibble

# I need doubles for later use of Spatial Data 
gps_coord_raw %>%
  as_tibble(.) %>%
  type_convert(.) %>%
  select(nr_punktu = NR_PUNKTU,
         lat = SZEROKOSC,
         lon = DLUGOSC) -> gps_coord

# I am loading site description data
sites_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>% # changing all columns to lower case
  select(nr_punktu, nr_cyklu, nr_podpow, rok_w_cyklu, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
         b_pion_pow_pr, tsl, okr_tsl, stan_siedl) %>%
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO") %>%
  select(-nr_cyklu) %>%
  type_convert(., col_types = cols_only(gat_pan_pr = col_factor(levels = NULL))) -> sites # converting column to factor

# loading tree data for the next script
trees_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>%
  select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
  dplyr::filter(nr_cyklu == 2, war == 1) %>%
  type_convert(., col_types = cols(nr_punktu = col_integer()))-> trees

# sample for WMS_GetFeatureInfo.R testing -----
# write_tsv(sample_n(gps_coord, 100), "gps.coord.sample.txt")

# joining files -----
sites_area <- dplyr::left_join(sites, plot_a, by = "nr_podpow") # joining sample plot area to site description
sites_area_gps <- dplyr::left_join(sites_area, gps_coord, by = "nr_punktu") # adding GPS position data

# exporting data ----- 
write_tsv(sites_area_gps, "sites_area_gps.txt") # saving tabular format for later analysis
write_tsv(trees, "trees.txt")