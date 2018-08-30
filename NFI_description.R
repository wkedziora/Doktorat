########################################
### NFI description for PhD research ###
########################################s

library(tidyverse)
library(feather)
library(tmap)
library(rgdal)
library(raster)
library(sp)
library(finalfit)

# data loading -----
sites <- read_feather(paste0(getwd(), "/data/WISL/sites.feather")) ### comment one
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))
area <- read_feather(paste0(getwd(), "/data/WISL/area.feather"))
gps <- read_feather(paste0(getwd(), "/data/WISL/gps.feather"))

# sample plot data wrangling -----
sites %>% summarise(n = n_distinct(nr_punktu))
sites %>% summarise(n = n_distinct(nr_podpow))

 # species
sites %>% 
        mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 10, other_level = "Inne")) %>%
        group_by(species = gat_pan_pr) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        mutate(species = fct_reorder(species, proc, .desc = TRUE)) %>%
        # mutate(species = fct_infreq(species)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = species, y = proc)) + 
        geom_col() + 
        labs(x = "gatunki", y = "[%]") + 
        # theme_minimal() +
        scale_x_discrete(na.value = "red")
# NA to głównie szczątkowe/dopełniające powierzchnie próbne

# habiatats
sites %>% 
        group_by(tsl) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        mutate(tsl = fct_reorder(tsl, -proc)) %>%
        mutate(tsl = fct_lump(tsl, n = 5)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = tsl, y = proc)) + 
        geom_col() + 
        labs(x = "siedliska", y = "[%]") + 
        theme_minimal()

# fertility
sites %>% 
        group_by(zyzn) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = zyzn, y = proc)) + 
        geom_col() + 
        labs(x = "żyzność", y = "[%]") + 
        theme_minimal()

#humidity1
sites %>% 
        group_by(wilg1) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = wilg1, y = proc)) + 
        geom_col() + 
        labs(x = "wilgotność", y = "[%]") + 
        theme_minimal()

#humidity2
sites %>% 
        group_by(wilg2) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        arrange(desc(n)) %>%
        ggplot(., aes(x = wilg2, y = proc)) + 
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

### SPATIAL PATTERN OF MISSING DATA ---------------------------------------------------------------------------
# sites_gps <- dplyr::left_join(sites, gps, by = "nr_punktu") #%>% na.omit()
sites_gps <- sites %>% 
        mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 10, other_level = "Inne")) %>%
        # filter(is.na(gat_pan_pr)) %>% # only for map of NA's
        dplyr::left_join(., gps, by = "nr_punktu")

### backup write/load site_index_gps ---------------------------------------------------------------------------
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

coordinates(sites_gps) <- ~ dlugosc + szerokosc #adding sptial relationship
proj4string(sites_gps) <- "+init=epsg:4326" #adding WGS84 projection

## ----- region-map ---------------------------------------------------------------------------
Region <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "Region")
Poland <- subset(Region, COUNTRY == "Poland")
plot_map_dots <- function(shape, feature) {
        tm_shape(Region, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
                tm_shape(shape) + tm_dots(col = feature, size = 0.5, n = 5) +
                tm_style("white", legend.position = c("left", "bottom"))
}

plot_map_dots(sites_gps, "gat_pan_pr")

bb_hire <- as(extent(sites_gps), "SpatialPolygons")
crs(bb_hire) <- crs(sites_gps)

r <- raster(bb_hire, ncol = 100, nrow = 100)
rc <- rasterize(sites_gps@coords, r, fun = "count")
plot(rc)
points(sites_gps)
plot(Poland, add = TRUE)

tm_shape(rc) + tm_raster() +
        tm_shape(Region, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders()

### checking for missing data ---------------------------------------------------------------------------
dependent <- c("plot_species")
# explanatory <- c("plot_age", "habitat", "habitat_status", "kw", "ownership")
explanatory <- c("kw", "owner")
sites %>% ff_glimpse()

# Identify missing values in each variable
sites %>% missing_plot()

# Look for patterns of missingness
sites %>% missing_pattern(dependent, explanatory)
# sites %>% summary_factorlist(dependent, explanatory, na_include = TRUE, p = TRUE)

# Check for associations between missing and observed data
sites %>% missing_pairs(dependent, explanatory)
sites %>% missing_pairs(dependent, explanatory, position = "fill")
# sites %>% finalfit(dependent, explanatory, metrics=TRUE)

# checking the same for "trees"
dependent_trees <- c("age")
# explanatory <- c("plot_age", "habitat", "habitat_status", "kw", "ownership")
explanatory_trees <- c("h", "owner", "kw", "dbh", "fertility")
trees %>% ff_glimpse()

# Identify missing values in each variable
trees %>% missing_plot()

# Look for patterns of missingness
trees %>% missing_pattern(dependent_trees, explanatory_trees)
# trees %>% summary_factorlist(dependent, explanatory, na_include = TRUE, p = TRUE)

# Check for associations between missing and observed data
trees %>% missing_pairs(dependent_trees, explanatory_trees)
trees %>% missing_pairs(dependent_trees, explanatory_trees, position = "fill")
trees %>% finalfit(dependent_trees, explanatory_trees, metrics = TRUE)
