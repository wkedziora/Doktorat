library(ggplot2)
library(grid)

`%||%` <- function (x, y)  if (is.null(x))  y else x

geom_marginboxplot <- function(mapping = NULL, data = NULL,
                               ...,
                               sides = "bl",
                               outlier.shape = 16,
                               outlier.size = 1.5,
                               outlier.stroke = 0.5,
                               width = 0.9,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
        
        layer(
                data = data,
                mapping = mapping,
                stat = StatMarginBoxplot,
                geom = GeomMarginBoxplot,
                position = "identity",
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(
                        sides = sides,
                        outlier.shape = outlier.shape,
                        outlier.size = outlier.size,
                        outlier.stroke = outlier.stroke,
                        width = width,
                        notch = FALSE,
                        notchwidth = 0.5,
                        varwidth = FALSE,
                        na.rm = na.rm,
                        ...
                )
        )
}

StatMarginBoxplot <- ggproto(
        "StatMarginBoxplot", Stat,
        optional_aes = c("x", "y"),
        non_missing_aes = "weight",
        
        setup_data = function(data, params, 
                              sides = "bl") {
                if(grepl("l|r", sides)){
                        data.vertical <- data
                        data.vertical$orientation <- "vertical"
                } else data.vertical <- data.frame()
                if(grepl("b|t", sides)){
                        data.horizontal <- data
                        data.horizontal$y <- data.horizontal$x
                        data.horizontal$orientation <- "horizontal"
                } else data.horizontal <- data.frame()
                data <- remove_missing(rbind(data.vertical, 
                                             data.horizontal),
                                       na.rm = FALSE, vars = "x", 
                                       "stat_boxplot")
                data
        },
        
        compute_group = function(data, scales, sides = "bl", 
                                 width = 0.9, na.rm = FALSE, coef = 1.5){
                
                if(grepl("l|r", sides)){
                        df.vertical <- do.call(environment(StatBoxplot$compute_group)$f,
                                               args = list(data = data[data$orientation == "vertical", ], 
                                                           scales = scales, width = width,
                                                           na.rm = na.rm, coef = coef))
                        df.vertical <- df.vertical[, c("ymin", "lower", "middle", "upper", "ymax", "outliers")]
                        df.vertical$orientation = "vertical"
                } else df.vertical <- data.frame()
                if(grepl("b|t", sides)){
                        df.horizontal <- do.call(environment(StatBoxplot$compute_group)$f,
                                                 args = list(data = data[data$orientation == "horizontal", ], 
                                                             scales = scales, width = width,
                                                             na.rm = na.rm, coef = coef))
                        df.horizontal <- df.horizontal[, c("ymin", "lower", "middle", "upper", "ymax", "outliers")]
                        df.horizontal$orientation = "horizontal"
                } else df.horizontal <- data.frame()
                
                df <- rbind(df.vertical, df.horizontal)
                
                colnames(df) <- gsub("^y", "", colnames(df))
                df
        }
)

GeomMarginBoxplot <- ggproto(
        "GeomMarginBoxplot", Geom,
        
        setup_data = function(data, params, sides = "bl") {
                
                data.vertical <- data[data$orientation == "vertical", ]
                if(nrow(data.vertical) > 0) {
                        colnames(data.vertical)[1:6] <- paste0("y", colnames(data.vertical)[1:6])
                } 
                data.horizontal <- data[data$orientation == "horizontal", ]
                if(nrow(data.horizontal) > 0){
                        colnames(data.horizontal)[1:6] <- paste0("x", colnames(data.horizontal)[1:6])
                }
                data <- merge(data.vertical, data.horizontal, all = TRUE)
                data <- data[, sapply(data, function(x) !all(is.na(x)))]
                data
        },
        
        draw_group = function(data, panel_params, coord, fatten = 2,
                              outlier.shape = 19, outlier.stroke = 0.5,
                              outlier.size = 1.5, width = 0.9,
                              notch = FALSE, notchwidth = 0.5, varwidth = FALSE,
                              sides = "bl") {
                
                draw.marginal.box <- function(sides){
                        
                        if(sides %in% c("l", "b")){
                                pos1 <- unit(0, "npc"); pos2 <- unit(0.03, "npc")
                        } else {
                                pos2 <- unit(0.97, "npc"); pos1 <- unit(1, "npc")
                        }
                        if(width > 0 & width < 1){
                                increment <- (1 - width) / 2
                                increment <- increment * (pos2 - pos1)
                                pos1 <- pos1 + increment
                                pos2 <- pos2 - increment
                        }
                        pos3 <- 0.5 * pos1 + 0.5 * pos2
                        
                        outliers_grob <- NULL
                        
                        if(sides %in% c("l", "r")) {
                                data <- data[data$orientation == "vertical", ]
                                
                                if (!is.null(data$youtliers) && length(data$youtliers[[1]] >= 1)) {
                                        
                                        outliers <- data.frame(
                                                y = unlist(data$youtliers[[1]]),
                                                x = 0,
                                                colour = data$colour[1],
                                                fill = data$fill[1],
                                                shape = outlier.shape %||% data$shape[1],
                                                size = outlier.size %||% data$size[1],
                                                stroke = outlier.stroke %||% data$stroke[1],
                                                alpha = data$alpha[1],
                                                stringsAsFactors = FALSE
                                        )
                                        
                                        coords <- coord$transform(outliers, panel_params)
                                        
                                        x.pos <- rep(pos3, nrow(coords))
                                        y.pos <- unit(coords$y, "native")
                                        
                                        outliers_grob <- pointsGrob(
                                                x = x.pos, y = y.pos,
                                                pch = coords$shape,
                                                gp = gpar(col = coords$colour, 
                                                          fill = alpha(coords$fill, coords$alpha), 
                                                          fontsize = coords$size * .pt + coords$stroke * .stroke/2, 
                                                          lwd = coords$stroke * .stroke/2))
                                }
                                
                                box.whiskers <- data.frame(
                                        y = c(data$ymin, data$ylower, data$ymiddle, data$yupper, data$ymax),
                                        x = 0,
                                        colour = data$colour[1],
                                        fill = data$fill[1],
                                        size = data$size[1],
                                        alpha = data$alpha[1],
                                        stringsAsFactors = FALSE
                                )
                                
                                box.whiskers <- coord$transform(box.whiskers, panel_params)
                                
                                whiskers_grob <- segmentsGrob(
                                        x0 = rep(pos3, 2),
                                        x1 = rep(pos3, 2),
                                        y0 = unit(c(box.whiskers$y[1], box.whiskers$y[5]), "native"),
                                        y1 = unit(c(box.whiskers$y[2], box.whiskers$y[4]), "native"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                                
                                box_grob <- rectGrob(
                                        x = pos1,
                                        y = unit(box.whiskers$y[4], "native"),
                                        width = pos2 - pos1,
                                        height = unit(box.whiskers$y[4] - box.whiskers$y[2], "native"),
                                        just = c("left", "top"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  fill = alpha(box.whiskers$fill, box.whiskers$alpha),
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                                
                                median_grob <- segmentsGrob(
                                        x0 = rep(pos1, 2),
                                        x1 = rep(pos2, 2),
                                        y0 = unit(box.whiskers$y[3], "native"),
                                        y1 = unit(box.whiskers$y[3], "native"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                        } 
                        
                        if(sides %in% c("b", "t")) {
                                data <- data[data$orientation == "horizontal", ]
                                
                                if (!is.null(data$xoutliers) && length(data$xoutliers[[1]] >= 1)) {
                                        
                                        outliers <- data.frame(
                                                x = unlist(data$xoutliers[[1]]),
                                                y = 0,
                                                colour = data$colour[1],
                                                fill = data$fill[1],
                                                shape = outlier.shape %||% data$shape[1],
                                                size = outlier.size %||% data$size[1],
                                                stroke = outlier.stroke %||% data$stroke[1],
                                                alpha = data$alpha[1],
                                                stringsAsFactors = FALSE
                                        )
                                        
                                        coords <- coord$transform(outliers, panel_params)
                                        
                                        x.pos <- unit(coords$x, "native")
                                        y.pos <- rep(pos3, nrow(coords))
                                        
                                        outliers_grob <- pointsGrob(
                                                x = x.pos, y = y.pos,
                                                pch = coords$shape,
                                                gp = gpar(col = coords$colour, 
                                                          fill = alpha(coords$fill, coords$alpha), 
                                                          fontsize = coords$size * .pt + coords$stroke * .stroke/2, 
                                                          lwd = coords$stroke * .stroke/2))
                                }
                                
                                box.whiskers <- data.frame(
                                        x = c(data$xmin, data$xlower, data$xmiddle, data$xupper, data$xmax),
                                        y = 0,
                                        colour = data$colour[1],
                                        fill = data$fill[1],
                                        size = data$size[1],
                                        alpha = data$alpha[1],
                                        stringsAsFactors = FALSE
                                )
                                
                                box.whiskers <- coord$transform(box.whiskers, panel_params)
                                
                                whiskers_grob <- segmentsGrob(
                                        y0 = rep(pos3, 2),
                                        y1 = rep(pos3, 2),
                                        x0 = unit(c(box.whiskers$x[1], box.whiskers$x[5]), "native"),
                                        x1 = unit(c(box.whiskers$x[2], box.whiskers$x[4]), "native"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                                
                                box_grob <- rectGrob(
                                        y = pos2,
                                        x = unit(box.whiskers$x[2], "native"),
                                        height = pos2 - pos1,
                                        width = unit(box.whiskers$x[4] - box.whiskers$x[2], "native"),
                                        just = c("left", "top"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  fill = alpha(box.whiskers$fill, box.whiskers$alpha),
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                                
                                median_grob <- segmentsGrob(
                                        y0 = rep(pos1, 2),
                                        y1 = rep(pos2, 2),
                                        x0 = unit(box.whiskers$x[3], "native"),
                                        x1 = unit(box.whiskers$x[3], "native"),
                                        gp = gpar(col = box.whiskers$colour,
                                                  lwd = box.whiskers$size * .pt,
                                                  lty = box.whiskers$linetype))
                        }
                        
                        grobTree(outliers_grob,
                                 whiskers_grob,
                                 box_grob,
                                 median_grob)
                }
                
                result <- list()
                
                if(grepl("l", sides)) result$l <- draw.marginal.box("l")
                if(grepl("r", sides)) result$r <- draw.marginal.box("r")
                if(grepl("b", sides)) result$b <- draw.marginal.box("b")
                if(grepl("t", sides)) result$t <- draw.marginal.box("t")
                
                gTree(children = do.call("gList", result))
                
        },
        
        draw_key = draw_key_boxplot,
        
        default_aes = aes(weight = 1, colour = "grey20", fill = "white", 
                          size = 0.5, stroke = 0.5,
                          alpha = 0.75, shape = 16, linetype = "solid",
                          sides = "bl"),
        
        optional_aes = c("lower", "upper", "middle", "min", "max")
)

st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "Region") %>% filter(COUNTRY == "Poland") -> Poland.shp
Poland.shp <- as(Poland.shp, "Spatial")
krainy.shp <- st_read(dsn = "D:\\Praca\\Badania\\Doktorat\\shp", layer = "krainy")

plot_map_dots <- function(shape, 
                          feature, 
                          breaks = c(-Inf, 14, 18, 22, 26, 30, Inf), 
                          t = "SI [m]", 
                          hist_h = 0.22, 
                          hist_show = TRUE) {
        # tm_shape(Poland.shp, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
        tm_shape(krainy.shp, projection="longlat", is.master = TRUE) + 
                tm_borders() +
                tm_shape(shape) + 
                tm_dots(col = feature, size = 0.035, palette = "RdYlGn", style="fixed", breaks = breaks, title = t, legend.hist = hist_show) +
                tm_style("white", title = "", legend.position = c("left", "bottom"), legend.stack = "horizontal", 
                         legend.format = list(text.separator= "-", text.less.than = "    <", text.or.more = "<")) + 
                tm_layout(legend.title.size = 1.2, legend.text.size = 0.8, 
                          inner.margins = c(0.08, 0.01, 0.01, 0.01), outer.margins = c(0, 0, 0, 0),
                          legend.hist.height = hist_h, frame = FALSE, 
                          legend.width = -0.5)
}

plot_map_polys <- function(shape, 
                           feature, 
                           t = "SI [m]") {
        tm_shape(krainy.shp) + tm_polygons(col = "white") +
                tm_shape(shape) + tm_polygons(feature, palette = "Greys", style = "pretty", title = t) + 
                tm_text(feature, size = 2, ymod = 1) +
                tm_style("white", legend.position = c("left", "bottom")) +
                tm_layout(legend.title.size = 1.2, legend.text.size = 0.8, fontface = 6,
                          inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0, 0, 0, 0))
}