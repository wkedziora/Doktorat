########################################
### NFI description for PhD research ###
########################################s

library(tidyverse)
library(feather)
library(tmap)
library(rgdal)
library(sp)

# data loading -----
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather")) ### comment one
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))

# sample plot data wrangling -----
sites %>% summarise(n = n_distinct(plot_no))
sites %>% summarise(n = n_distinct(subplot_no))

 # species
sites %>% 
        group_by(species = plot_species) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        mutate(species = fct_reorder(species, -proc)) %>%
        mutate(species = fct_lump(species, n = 5)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = species, y = proc)) + 
        geom_col() + 
        labs(x = "gatunki", y = "[%]") + 
        theme_minimal() 
# NA to głównie szczątkowe/dopełniające powierzchnie próbne

# habiatats
sites %>% 
        group_by(habitat = habitat) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        mutate(habitat = fct_reorder(habitat, -proc)) %>%
        mutate(habitat = fct_lump(habitat, n = 5)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = habitat, y = proc)) + 
        geom_col() + 
        labs(x = "siedliska", y = "[%]") + 
        theme_minimal()

# fertility
sites %>% 
        group_by(fertility = fertility) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = fertility, y = proc)) + 
        geom_col() + 
        labs(x = "żyzność", y = "[%]") + 
        theme_minimal()

#humidity1
sites %>% 
        group_by(humidity1 = humidity1) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = humidity1, y = proc)) + 
        geom_col() + 
        labs(x = "wilgotność", y = "[%]") + 
        theme_minimal()

#humidity2
sites %>% 
        group_by(humidity2 = humidity2) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = humidity2, y = proc)) + 
        geom_col() + 
        labs(x = "wilgotność w grupach", y = "[%]") + 
        theme_minimal()

#klasa wieku
sites %>% 
        group_by(kw = kw) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = kw, y = proc)) + 
        geom_col() + 
        labs(x = "klasa wieku", y = "[%]") + 
        theme_minimal()

## ----- gps ----------------------------------------------------------------------------------------
sites_gps <- dplyr::left_join(sites, gps, by = "plot_no") #%>% na.omit()

### backup write/load site_index_gps ---------------------------------------------------------------------------------
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

coordinates(sites_gps) <- ~ lon + lat #adding sptial relationship
proj4string(sites_gps) <- "+init=epsg:4326" #adding WGS84 projection

## ----- region-map ----------------------------------------------------------------------------------------
data(World, rivers)
# vistula <- subset(rivers, name == "Vistula")
Poland <- subset(World, name == "Poland")
plot_map_dots <- function(shape, feature) {
        tm_shape(World, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
                # tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
                tm_shape(shape) + tm_dots(col = feature, size = 0.05, n = 5) +
                tm_style("white", legend.position = c("left", "bottom"))
}

plot_map_dots(sites_gps, "plot_species")
