########################################
### NFI description for PhD research ###
########################################

### library loading ---------------------------------------------------------------------------
library(tidyverse)
library(feather)
library(tmap)
library(rgdal)
library(raster)
library(sp)
library(sf)
library(finalfit)

# data loading --------------------------------------------------------------------------------
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather")) ### comment one
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))

trees %>% 
        group_by(nr_punktu, nr_podpow) %>% 
        summarise(n_drzew = n(), n_gat = n_distinct(gat)) %>% 
        dplyr::inner_join(., area, by = "nr_podpow", suffix = c("", ".y")) %>% 
        dplyr::inner_join(., gps, by = "nr_punktu", suffix = c("", ".y")) %>% 
        dplyr::inner_join(., sites, by = "nr_podpow", suffix = c("", ".y")) %>% 
        dplyr::select(-c(nr_punktu.y, rok_w_cyklu, okr_tsl, stan_siedl, kodn, zyzn, grp, wilg1, wilg2, wlasc)) -> RT

write_csv2(RT, "RT.csv")
