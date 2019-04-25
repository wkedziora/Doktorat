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

theme_set(
        theme_bw() +
                theme(legend.position = "none", 
                      axis.text = element_text(size = 11), 
                      axis.title = element_text(size = 12, face = "bold"))
)

# sites <- sites %>% mutate_if(is.factor,
#                     fct_explicit_na,
#                     na_level = "(b.d.)")

# sample plot data wrangling -------------------------------------------------------------------
sites %>% summarise(n = n_distinct(nr_punktu))
sites %>% summarise(n = n_distinct(nr_podpow))

# krainy
sites %>% group_by(kraina = kraina) %>% summarise(n = n_distinct(nr_punktu))
sites %>% group_by(kraina = kraina) %>% summarise(n = n_distinct(nr_podpow))

sites %>% 
        mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 7, other_level = "inne")) %>%
        drop_na(gat_pan_pr) %>% 
        group_by(species = gat_pan_pr) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = species, y = proc, fill = "lightgrey")) + 
        geom_col(color = "black") + 
        labs(x = "gatunki", y = "[%]") + 
        scale_fill_manual(values = "lightgrey")
        
ggsave("img/NFI_species_barplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# w krainach
sites$kraina <- fct_recode(sites$kraina, "kraina I" = "1", "kraina II" = "2", "kraina III" = "3", "kraina IV" = "4", 
                                "kraina V" = "5", "kraina VI" = "6", "kraina VII" = "7", "kraina VIII" = "8")

sites %>% 
        mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 7, other_level = "Inne")) %>%
        drop_na(gat_pan_pr) %>% 
        group_by(kraina = kraina, species = gat_pan_pr) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = species, y = proc, fill = "lightgrey")) + 
        geom_col(color = "black") + 
        labs(x = "gatunki", y = "[%]") + 
        facet_wrap(~kraina, nrow = 4, ncol = 2) +
        scale_fill_manual(values = "lightgrey")

ggsave("img/NFI_species_barplot_facet.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

# # habiatats - too detailed information, non-digestable
# sites %>% 
#         # filter(!is.na(tsl)) %>%
#         group_by(tsl) %>% 
#         summarise(n = n()) %>% 
#         mutate(proc = 100*n/sum(n)) %>%
#         mutate(tsl = fct_reorder(tsl, -proc)) %>%
#         mutate(tsl = fct_lump(tsl, n = 5)) %>%
#         arrange(desc(n)) %>%
#         ggplot(., aes(x = tsl, y = proc)) + 
#         geom_col() + 
#         labs(x = "siedliska", y = "[%]") + 
#         theme_minimal()

# fertility
sites %>% 
        group_by(zyzn) %>% 
        drop_na(zyzn) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = zyzn, y = proc, fill = "lightgrey")) +
        geom_col(colour = "black") + 
        labs(x = "żyzność", y = "[%]") +
        scale_fill_manual(values = "lightgrey")


ggsave("img/NFI_zyzn_barplot.png", device = png(), width = 19, height = 15, units = "cm", dpi = 500)
dev.off()

sites %>%
        group_by(kraina, zyzn) %>%
        drop_na(kraina, zyzn) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = zyzn, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "żyzność", y = "[%]") + 
        facet_wrap(~kraina, nrow = 4, ncol = 2) + 
        # scale_x_discrete(labels = c("B", "", "LM", "", "BMwyż", "", "Lwyż", "", "BMG", "", "LG", "")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        scale_fill_manual(values = "lightgrey")

ggsave("img/NFI_zyzn_barplot_facet.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

#humidity1
sites %>% 
        group_by(wilg1) %>% 
        drop_na(wilg1) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = wilg1, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "wilgotność", y = "[%]") +
        scale_fill_manual(values = "lightgrey")

ggsave("img/NFI_wilg_barplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

sites %>% 
        group_by(kraina, wilg1) %>% 
        drop_na(kraina, wilg1) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = wilg1, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "wilgotność", y = "[%]") + 
        scale_fill_manual(values = "lightgrey") +
        facet_wrap(~kraina, nrow = 4, ncol = 2)

ggsave("img/NFI_wilg_barplot_facet.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

#humidity2
sites %>% 
        group_by(wilg2) %>% 
        drop_na(wilg2) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = wilg2, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "wilgotność", y = "[%]") +
        scale_fill_manual(values = "lightgrey") 

ggsave("img/NFI_wilg2_barplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

sites %>% 
        group_by(kraina, wilg2) %>% 
        drop_na(kraina, wilg2) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = wilg2, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "wilgotność", y = "[%]") + 
        scale_fill_manual(values = "lightgrey") +
        facet_wrap(~kraina, nrow = 4, ncol = 2) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("img/NFI_wilg2_barplot_facet.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

#klasa wieku
sites %>% 
        group_by(kw) %>% 
        drop_na(kw) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = kw, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "klasa wieku", y = "[%]") +
        scale_fill_manual(values = "lightgrey") 
        
ggsave("img/NFI_kw_barplot.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

sites %>% 
        group_by(kraina, kw) %>% 
        drop_na(kw) %>% 
        summarise(n = n()) %>% 
        mutate(proc = 100*n/sum(n)) %>%
        ggplot(., aes(x = kw, y = proc, fill = "lightgrey")) + 
        geom_col(colour = "black") + 
        labs(x = "klasa wieku", y = "[%]") + 
        scale_fill_manual(values = "lightgrey") +
        facet_wrap(~kraina, nrow = 4, ncol = 2)

ggsave("img/NFI_kw_barplot_facet.png", device = png(), width = 15, height = 15, units = "cm", dpi = 500)
dev.off()

### SPATIAL PATTERN OF MISSING DATA ------------------------------------------------------------
# sites_gps <- dplyr::left_join(sites, gps, by = "nr_punktu") #%>% na.omit()
sites_gps <- sites %>% 
        mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 10, other_level = "Inne")) %>%
        # filter(is.na(gat_pan_pr)) %>% # only for map of NA's
        dplyr::left_join(., gps, by = "nr_punktu") %>%
        st_as_sf(., coords = c("dlugosc", "szerokosc"), crs = "+init=epsg:4326") %>% 
        st_transform(., 2180)

### backup write/load site_index_gps ------------------------------------------------------------
# write_feather(site_index_gps, paste0(getwd(), "/data/WISL/site_index_gps.feather"))
# site_index_gps <- read_feather(paste0(getwd(), "/data/WISL/site_index_gps.feather"))

# coordinates(sites_gps) <- ~ dlugosc + szerokosc #adding sptial relationship
# proj4string(sites_gps) <- "+init=epsg:4326" #adding WGS84 projection

## ----- region-map ---------------------------------------------------------------------------
Region <- readOGR(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "Region")
Poland <- subset(Region, COUNTRY == "Poland")
plot_map_dots <- function(shape, feature) {
        tm_shape(Region, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
                tm_shape(shape) + tm_dots(col = feature, size = 0.1, n = 5) +
                tm_style("white", legend.position = c("left", "bottom"))
}

plot_map_dots(sites_gps %>% filter(is.na(wlasc)), "wlasc")

frequency <- as(extent(sites_gps), "SpatialPolygons")
crs(frequency) <- crs("+init=epsg:2180")

r_frequency <- raster(frequency, resolution = 10000)
rc_frequency <- rasterize(st_coordinates(sites_gps), r_frequency, fun = "count")
freq <- tm_shape(rc_frequency) + tm_raster(palette = "Greys", title = "Zagęszczenie \n[pow/100 km^2]") +
        tm_shape(Region, bbox = "Poland", is.master = TRUE) + tm_borders() +
        tm_legend(position = c("left", "bottom"))

tmap_save(freq, filename = "mapy/WISL_freq.png", dpi = 600, width = 15, height = 14, units = "cm")


### checking for missing data --------------------------------------------------------------------
# sites %>% ff_glimpse()

# Identify missing values in each variable
# sites %>% missing_plot()

# Look for patterns of missingness
sites %>% rename("gatunek \npanujący" = "gat_pan_pr",
                 "siedliskowy \ntyp lasu" = "tsl",
                 "klasa \nwieku" = "kw",
                 "własność" = "wlasc") %>% 
        missing_pattern(explanatory = c("gatunek \npanujący", "siedliskowy \ntyp lasu", "klasa \nwieku", "własność"))
# sites %>% summary_factorlist(dependent, explanatory, na_include = TRUE, p = TRUE)

# Check for associations between missing and observed data
# sites %>% missing_pairs(dependent, explanatory)
explanatory <- c("zyzn", "wilg1", "kw", "wlasc")
sites %>% 
        # mutate(gat_pan_pr = fct_lump(gat_pan_pr, n = 7, other_level = "Inne")) %>% # in case you want to reorder sth
        missing_pairs(dependent, explanatory, position = "fill")

# sites %>% finalfit(dependent, explanatory, metrics = TRUE) # not working???
sites %>% finalfit("kw", "tsl", metrics = TRUE)
# checking the same for "trees"
dependent_trees <- c("wiek")
# explanatory <- c("plot_age", "habitat", "habitat_status", "kw", "ownership")
explanatory_trees <- c("h", "d13") # kw na powierzchni próbnej
# trees %>% ff_glimpse()

# Identify missing values in each variable
# trees %>% missing_plot()

# Look for patterns of missingness
trees %>% missing_pattern(dependent_trees, explanatory_trees)
# trees %>% summary_factorlist(dependent, explanatory, na_include = TRUE, p = TRUE)

# Check for associations between missing and observed data
# trees %>% missing_pairs(dependent_trees, explanatory_trees)
trees %>% missing_pairs(dependent_trees, explanatory_trees, position = "fill")
# trees %>% finalfit(dependent_trees, explanatory_trees, metrics = TRUE)


# test <- sites %>% left_join(., area, by = "nr_podpow") %>% group_by(tsl) %>% summarise(pow = mean(pow_a))
# ggplot(sites %>% left_join(., area, by = "nr_podpow")) +
#         geom_boxplot(aes(x = wlasc, y = pow_a))

#EOF