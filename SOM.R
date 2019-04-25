#############################################
### Self-Organising Maps for PhD research ###
#############################################

library(raster)
library(sf)
library(tidyverse)
library(corrplot)
library(feather)

# write_feather(data, paste0(getwd(), "/data/WISL/si_factors.feather"))
si_factors <- read_feather(paste0(getwd(), "/data/WISL/si_factors.feather"))

### CORRELATION ---------------------------------------------------------------------------
# correlation calculation 
res_p <- cor(si_factors[-c(2:10)], use = "complete.obs", method = c("pearson"))
# res_k <- cor(data, use = "complete.obs", method = c("kendall")) #long-running
res_s <- cor(si_factors[-c(2:10)], use = "complete.obs", method = c("spearman"))

# plotting correlations
corrplot(res_p, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# corrplot(res_k, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res_s, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

### Self-Organising Maps ------------------------------------------------------------------
library(kohonen)
library(RColorBrewer)

colnames(si_factors)

si_factors.measures1 <- c("bio01", "bio21", "tri")
si_factors.SOM1 <- som(scale(si_factors[si_factors.measures1]), grid = somgrid(20, 20, "hexagonal"))
plot(si_factors.SOM1)

# reverse color ramp
colors <- function(n, alpha = 1) {
        rev(heat.colors(n, alpha))
}

par(mfrow = c(1, 2))
plot(si_factors.SOM1, type = "counts", palette.name = colors, heatkey = TRUE, main = "Mapping Type SOM")
plot(si_factors.SOM1, main = "Default SOM Plot")

# Create a training data set (rows are samples, columns are variables
# Here I am selecting a subset of my variables available in "data"
si_factors_train <- si_factors[, c(13:65)]
# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
si_factors_train_matrix <- as.matrix(scale(si_factors_train))
# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")
# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(si_factors_train_matrix, 
                 grid = som_grid, 
                 rlen = 500, 
                 alpha = c(0.05,0.01), 
                 keep.data = TRUE)

#Training progress for SOM
plot(som_model, type="changes")

#Node count plot
plot(som_model, type="count", main="Node Counts")

# U-matrix visualisation
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")

# Weight Vector View
plot(som_model, type="codes")

# Kohonen Heatmap creation
plot(som_model, type = "property", property = getCodes(som_model)[,4], 
     main = colnames(getCodes(som_model))[4])

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

