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
trees_raw <- dbReadTable(con, "DRZEWA_OD_7") #tree measurements
area_raw <- dbReadTable(con, "POW_A_B") #plot data
gps_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates
ownership_raw <- dbReadTable(con, "SL_R_WLASN_LS") # land ownership

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

# loading site description data
sites_raw %>%
  as_tibble(.) %>%
  dplyr::select(plot_no = NR_PUNKTU,
                cycle_no = NR_CYKLU,
                subplot_no = NR_PODPOW,
                cycle_year = ROK_W_CYKLU,
                region = KRAINA,
                rdlp = RDLP,
                nadl = NADL,
                plot_species = GAT_PAN_PR,
                plot_age = WIEK_PAN_PR,
                vertical = B_PION_POW_PR, 
                habitat = TSL, 
                habitat_source = OKR_TSL, 
                habitat_status = STAN_SIEDL,
                ownership = R_WLASN_LS) %>%
  dplyr::filter(cycle_no == 2) %>%
  dplyr::select(-cycle_no) %>%
  type_convert(col_types = cols_only(plot_species = col_factor(levels = NULL))) %>%
  mutate(habitat = factor(habitat),
         habitat_source = factor(habitat_source),
         habitat_status = factor(habitat_status),
         rdlp = str_pad(rdlp, 2, pad = "0"),
         nadl = str_pad(nadl, 2, pad = "0"),
         kodn = paste(rdlp, nadl, sep = "-")) %>%
  dplyr::select(-c(rdlp, nadl)) %>%
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
                                    habitat %in% c(93, 94, 95, 96) ~ "LG",
                                    habitat %in% c(18) ~ "BWG"),
              humidity1 = case_when(habitat %in% c(11) ~ "s",
                                   habitat %in% c(12, 22, 32, 42, 55, 61, 63, 71, 81, 91, 93, 18) ~ "św",
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
              fertility = ordered(fertility, levels = c("B", "BM", "LM", "L", "BMwyż", "LMwyż", "Lwyż", "BG", "BMG", "LMG", "LG", "BWG")),
              humidity1 = ordered(humidity1, levels = c("s", "św", "w", "b", "z")),
              humidity2 = ordered(humidity2, levels = c("Ns", "Nś", "Nw", "Nb", "Nz","Wś", "Ww", "Wz", "Gś", "Gw", "Gb", "Gz", "WGś")),
              habitat = ordered(habitat, levels = c("Bs", "Bśw", "Bw", "Bb", "BMśw", "BMw", "BMb", "LMśw", "LMw", "LMb", "Lśw", "Lw", 
                                                    "Ol", "OlJ", "Lł",  "BMwyżśw", "BMwyżw", "LMwyżśw", "LMwyżw", "Lwyżśw", "Lwyżw", 
                                                    "OlJwyż", "Lłwyż", "BGśw", "BGw", "BGb", "BMGśw", "BMGw", "BMGb", "LMGśw", "LMGw", 
                                                    "LGśw", "LGw", "OlJG", "LłG", "BWG"))) -> sites

levels(sites$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

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
  type_convert(., col_types = cols(plot_no = col_integer())) %>%
  type_convert(col_types = cols_only(species = col_factor(levels = NULL))) -> trees

### adding region to tree data
trees <- dplyr::inner_join(trees, sites, by = "subplot_no", suffix = c("", ".y")) %>% dplyr::select(-c(plot_no.y, cycle_year, plot_species, plot_age, vertical, habitat, habitat_source, habitat_status))

# ownership loading

ownership_raw %>%
        as_tibble(.) %>%
        dplyr::filter(NR_CYKLU == 2) %>%
        dplyr::select(code = R_WLASN_LS,
                      title = R_WLASN_LS_NAZWA) -> ownership

sites <- dplyr::left_join(sites, ownership, by = c("ownership" = "code")) %>% dplyr::select(-c(ownership), ownership = title)


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

