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
        dplyr::select(NR_PODPOW, POW_A) %>% # selecting only interesting columns
        dplyr::rename_all(tolower) -> area # creating a new tibble

# I need doubles for later use of Spatial Data 
gps_raw %>%
        as_tibble(.) %>%
        type_convert(.) %>%
        dplyr::select(NR_PUNKTU,SZEROKOSC, DLUGOSC) %>%
        dplyr::rename_all(tolower) -> gps

# loading site description data
sites_raw %>%
        as_tibble(.) %>%
        dplyr::select(NR_PUNKTU, NR_CYKLU, NR_PODPOW, ROK_W_CYKLU, KRAINA, RDLP, NADL, GAT_PAN_PR, WIEK_PAN_PR,
                      B_PION_POW_PR, TSL, OKR_TSL, STAN_SIEDL, R_WLASN_LS) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::filter(nr_cyklu == 2) %>%
        dplyr::select(-nr_cyklu) %>%
        dplyr::mutate(gat_pan_pr = str_to_title(gat_pan_pr)) %>%
        type_convert(col_types = cols_only(gat_pan_pr = col_factor(levels = c("So", "Md", "Św", "Jd", "Dg", "So.we", "So.k", "So.c", "So.b", 
                                                                              "Db", "Db.b", "Db.s", "Db.c", "Js", "Wz", "Ak", 
                                                                              "Bk", "Brz", "Brz.o", "Gb", "Lp", "Kl", "Jw", "Bst", "Ol", "Ol.s", "Wb", "Wb.no", "Iwa",
                                                                              "Tp", "Tp.c", "Os", "Jkl",
                                                                              "Lsz", "Czm", "Jrz", "Bez.c", "Czm.p", "Der.b", "Śl", "Głg", "Jb",  "Gr", "Czr",
                                                                              "Der.ś", "Cyp.l" )))) %>%
        mutate(tsl = factor(tsl),
               okr_tsl = factor(okr_tsl),
               stan_siedl = factor(stan_siedl),
               rdlp = str_pad(rdlp, 2, pad = "0"),
               nadl = str_pad(nadl, 2, pad = "0"),
               kodn = paste(rdlp, nadl, sep = "-")) %>%
        dplyr::select(-c(rdlp, nadl)) %>%
        dplyr::mutate(tsl = as.integer(levels(tsl))[tsl],
                      zyzn = case_when(tsl %in% c(11, 12, 13, 14) ~ "B",
                                       tsl %in% c(22, 23, 24) ~ "BM",
                                       tsl %in% c(32, 33, 34) ~ "LM",
                                       tsl %in% c(42, 43, 44, 45, 46) ~ "L",
                                       tsl %in% c(55, 56) ~ "BMwyż",
                                       tsl %in% c(61, 62) ~ "LMwyż",
                                       tsl %in% c(63, 64, 65, 66) ~ "Lwyż",
                                       tsl %in% c(71, 72, 73) ~ "BG",
                                       tsl %in% c(81, 82, 83) ~ "BMG",
                                       tsl %in% c(91, 92) ~ "LMG",
                                       tsl %in% c(93, 94, 95, 96) ~ "LG",
                                       tsl %in% c(18) ~ "BWG"),
                      wilg1 = case_when(tsl %in% c(11) ~ "s",
                                        tsl %in% c(12, 22, 32, 42, 55, 61, 63, 71, 81, 91, 93, 18) ~ "św",
                                        tsl %in% c(13, 23, 33, 43, 56, 62, 64, 72, 82, 92, 94) ~ "w",
                                        tsl %in% c(14, 24, 34, 44, 73, 83) ~ "b",
                                        tsl %in% c(45, 46, 65, 66, 95, 96) ~ "z"),
                      wilg2 = case_when(tsl %in% c(11) ~ "Ns",
                                        tsl %in% c(12, 22, 32, 42) ~ "Nś",
                                        tsl %in% c(13, 23, 33, 43) ~ "Nw",
                                        tsl %in% c(14, 24, 34, 44) ~ "Nb",
                                        tsl %in% c(45, 46) ~ "Nz",
                                        tsl %in% c(55, 61, 63) ~ "Wś",
                                        tsl %in% c(56, 62, 64) ~ "Ww",
                                        tsl %in% c(65, 66) ~ "Wz",
                                        tsl %in% c(71, 81, 91, 93) ~ "Gś",
                                        tsl %in% c(72, 82, 92, 94) ~ "Gw",
                                        tsl %in% c(73, 83) ~ "Gb",
                                        tsl %in% c(95, 96) ~ "Gz",
                                        tsl %in% c(18) ~ "WGś"),
                      tsl = case_when(tsl == 11 ~ "Bs",
                                        tsl == 12 ~ "Bśw",
                                        tsl == 13 ~ "Bw",
                                        tsl == 14 ~ "Bb",
                                        tsl == 18 ~ "BWG",
                                        tsl == 22 ~ "BMśw",
                                        tsl == 23 ~ "BMw",
                                        tsl == 24 ~ "BMb",
                                        tsl == 32 ~ "LMśw",
                                        tsl == 33 ~ "LMw",
                                        tsl == 34 ~ "LMb",
                                        tsl == 42 ~ "Lśw",
                                        tsl == 43 ~ "Lw",
                                        tsl == 44 ~ "Ol",
                                        tsl == 45 ~ "OlJ",
                                        tsl == 46 ~ "Lł",
                                        tsl == 55 ~ "BMwyżśw",
                                        tsl == 56 ~ "BMwyżw",
                                        tsl == 61 ~ "LMwyżśw",
                                        tsl == 62 ~ "LMwyżw",
                                        tsl == 63 ~ "Lwyżśw",
                                        tsl == 64 ~ "Lwyżw",
                                        tsl == 65 ~ "OlJwyż",
                                        tsl == 66 ~ "Lłwyż",
                                        tsl == 71 ~ "BGśw",
                                        tsl == 72 ~ "BGw",
                                        tsl == 73 ~ "BGb",
                                        tsl == 81 ~ "BMGśw",
                                        tsl == 82 ~ "BMGw",
                                        tsl == 83 ~ "BMGb",
                                        tsl == 91 ~ "LMGśw",
                                        tsl == 92 ~ "LMGw",
                                        tsl == 93 ~ "LGśw",
                                        tsl == 94 ~ "LGw",
                                        tsl == 95 ~ "LłG",
                                        tsl == 96 ~ "OlJG"),
                      kw = cut(wiek_pan_pr, breaks=seq(0, 260, by=20)),
                      kraina = factor(kraina),
                      zyzn = ordered(zyzn, levels = c("B", "BM", "LM", "L", "BMwyż", "LMwyż", "Lwyż", "BG", "BMG", "LMG", "LG", "BWG")),
                      wilg1 = ordered(wilg1, levels = c("s", "św", "w", "b", "z")),
                      wilg2 = ordered(wilg2, levels = c("Ns", "Nś", "Nw", "Nb", "Nz","Wś", "Ww", "Wz", "Gś", "Gw", "Gb", "Gz", "WGś")),
                      tsl = ordered(tsl, levels = c("Bs", "Bśw", "Bw", "Bb", "BMśw", "BMw", "BMb", "LMśw", "LMw", "LMb", "Lśw", "Lw", 
                                                        "Ol", "OlJ", "Lł",  "BMwyżśw", "BMwyżw", "LMwyżśw", "LMwyżw", "Lwyżśw", "Lwyżw", 
                                                        "OlJwyż", "Lłwyż", "BGśw", "BGw", "BGb", "BMGśw", "BMGw", "BMGb", "LMGśw", "LMGw", 
                                                        "LGśw", "LGw", "OlJG", "LłG", "BWG"))) -> sites

levels(sites$kw) <- c("I", "II", "III", "IV", "V", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st", "VI i st")

# ownership loading

ownership_raw %>%
        as_tibble(.) %>%
        dplyr::filter(NR_CYKLU == 2) %>%
        dplyr::select(kod = R_WLASN_LS,
                      wlasc = R_WLASN_LS_NAZWA) %>%
        dplyr::mutate(wlasc = case_when(kod == 1 ~ "LP",
                                        kod == 2 ~ "PN",
                                        kod %in% c(3, 9) ~ "SP",
                                        kod %in% c(4, 5, 6, 7, 8) ~ "pryw"),
                      wlasc = factor(wlasc)) -> ownership

sites <- dplyr::left_join(sites, ownership, by = c("r_wlasn_ls" = "kod")) %>% dplyr::select(-c(r_wlasn_ls))

# loading tree data for the next script
trees_raw %>%
        as_tibble(.) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
        dplyr::filter(nr_cyklu == 2, war == 1) %>%
        dplyr::select(-c(nr_cyklu, war)) %>%
        type_convert(., col_types = cols(nr_punktu = col_integer())) %>%
        type_convert(col_types = cols_only(gat = col_factor(levels = NULL))) -> trees

### adding region to tree data
trees <- dplyr::inner_join(trees, sites, by = "nr_podpow", suffix = c("", ".y")) %>% 
        dplyr::select(-c(nr_punktu.y, rok_w_cyklu, gat_pan_pr, wiek_pan_pr, b_pion_pow_pr, tsl, okr_tsl, stan_siedl))


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

#EOF
