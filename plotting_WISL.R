###########################################
### WISL data plotting for PhD research ###
###########################################

library(tidyverse)
library(ggrepel)
library(feather)

## ----- data_loading -------------------------------------------------------------------------
trees <- read_feather(paste0(getwd(), "/data/WISL/trees.feather"))

theme_set(
        theme_bw() +
                theme(panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "right", 
                      axis.text = element_text(size = 11), 
                      axis.title = element_text(size = 12, face = "bold")
                      )
)

tree_colours <- c(rgb(214, 134, 64, maxColorValue = 256), 
                  rgb(64, 230, 231, maxColorValue = 256), 
                  rgb(255, 191, 0, maxColorValue = 256), 
                  rgb(118, 147, 72, maxColorValue = 256), 
                  rgb(255, 191, 0, maxColorValue = 256), 
                  rgb(255, 135, 135, maxColorValue = 256), 
                  rgb(140, 140, 140, maxColorValue = 256))
names(tree_colours) <- c("SO", "BRZ", "GB", "WZ", "BK", "CZR", "DB")

trees %>% filter(nr_podpow == 38162301) %>% 
        ggplot(aes(x = azymut, y = odl, size = d13, color = gat, label = h)) +
        geom_point(alpha = 0.5) +
        coord_polar(theta = "x", start = 0) + 
        scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360-1, by = 45)) + 
        scale_size_continuous(name = "Pierśnica [mm]", range = c(1, 10)) +
        scale_colour_manual(name = "Gatunek", values = tree_colours) +
        geom_text_repel(aes(size = 100), point.padding = NA, show.legend = FALSE, na.rm = TRUE, segment.size = 1)

ggsave("img/WISL_plot.png", device = png(), width = 15, height = 14, units = "cm", dpi = 500)
dev.off()
  