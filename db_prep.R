#############################################
### Data import and tidy for PhD research ###
#############################################

library(odbc)
library(DBI)
library(tidyverse)
library(feather)

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
sites_raw <- dbReadTable(con, "ADRES_POW") #site description
trees_raw <- dbReadTable(con, "DRZEWA_OD_7") #tree measurments
area_raw <- dbReadTable(con, "POW_A_B") #plot data
gps_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates

# data wrangling -----
# I am querying for area of sample plot needed later
area_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  dplyr::filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  dplyr::select(subplot_no = NR_PODPOW, # selecting only interesitng colmuns
         area = POW_A) -> area # creating a new tibble

# I need doubles for later use of Spatial Data 
gps_raw %>%
  as_tibble(.) %>%
  type_convert(.) %>%
  dplyr::select(plot_no = NR_PUNKTU,
         lat = SZEROKOSC,
         lon = DLUGOSC) -> gps

# I am loading site description data
sites_raw %>%
  as_tibble(.) %>%
  dplyr::select(plot_no = NR_PUNKTU,
         cycle_no = NR_CYKLU,
         subplot_no = NR_PODPOW,
         cycle_year = ROK_W_CYKLU,
         region = KRAINA,
         plot_species = GAT_PAN_PR,
         plot_age = WIEK_PAN_PR,
         vertical = B_PION_POW_PR, 
         habitat = TSL, 
         habitat_source = OKR_TSL, 
         habitat_status = STAN_SIEDL) %>%
  dplyr::filter(cycle_no == 2, plot_species == "SO") %>%
  dplyr::select(-cycle_no) %>%
  type_convert(col_types = cols_only(plot_species = col_factor(levels = NULL))) %>%
  mutate(habitat = factor(habitat),
         habitat_source = factor(habitat_source),
         habitat_status = factor(habitat_status)) -> sites # converting column to factor

# loading tree data for the next script
trees_raw %>%
  as_tibble(.) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
  dplyr::filter(nr_cyklu == 2, war == 1) %>%
  dplyr::select(-c(nr_cyklu, war)) %>%
  dplyr::rename(plot_no = nr_punktu,
         subplot_no = nr_podpow,
         species = gat,
         age = wiek,
         azimuth = azymut,
         dist = odl,
         dbh = d13) %>%
  type_convert(., col_types = cols(plot_no = col_integer()))-> trees

### adding region to tree data
trees <- dplyr::inner_join(trees, sites, by = "subplot_no", suffix = c("", ".y")) %>% dplyr::select(-c(plot_no.y, cycle_year, plot_species, plot_age, vertical, habitat, habitat_source, habitat_status))

# sample for WMS_GetFeatureInfo.R testing -----
# write_tsv(sample_n(gps_coord, 100), "gps.coord.sample.txt")

# joining files -----
# sites_area <- dplyr::left_join(sites, plot_a, by = "nr_podpow") # joining sample plot area to site description
# sites_area_gps <- dplyr::left_join(sites_area, gps_coord, by = "nr_punktu") # adding GPS position data

# # exporting data ----- 
# write_tsv(sites, paste0(getwd(), "/data/sites.txt")) # saving tabular format for later analysis
# write_tsv(trees, paste0(getwd(), "/data/trees.txt"))
# write_tsv(area, paste0(getwd(), "/data/area.txt"))
# write_tsv(gps_coord, paste0(getwd(), "/data/gps_coord.txt"))

write_feather(sites, paste0(getwd(), "/data/WISL/sites.feather"))
write_feather(trees, paste0(getwd(), "/data/WISL/trees.feather"))
write_feather(area, paste0(getwd(), "/data/WISL/area.feather"))
write_feather(gps, paste0(getwd(), "/data/WISL/gps.feather"))

