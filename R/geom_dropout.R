# The ggproto() function is used to construct a new class corresponding to your new geom. 
# This new class specifies a number of attributes and functions that describe how data should be drawn on a plot.
# 
library(grid)
GeomDropout <- ggproto("GeomDropout", Geom,
                   required_aes = c("x", "y", "condition"), # such as x and y
                   default_aes = aes(linewidth = 1), # <default values for certain aesthetics>, plot symbol, color
                   draw_key = draw_key_abline, # <a function used to draw the key in the legend>
                   draw_panel = function(data, panel_scales, coord) {
                     ## Function that returns a grid grob that will 
                     ## be plotted (this is where the real work occurs)
                
                     
                     # Add the correct variables
                     x <- data$q_idx
                     
                     y <- data$pct_remain*100
                     
                     condition <- factor(data$condition)
                     
                     ## Transform the data to match the coordinate system
                     coords <- coord$transform(data, panel_scales)
                     
                     # needs some knowledge of the grid package
                     # The data element is a data frame containing one column for each aesthetic specified, 
                     # panel_scales is a list containing information about the x and y scales for the current panel, and 
                     # coord is an object that describes the coordinate system of your plot.
                     
                     ## Construct a grid grob
                     grid::linesGrob(
                       x = coords$x,
                       y = coords$y,
                       # pch = coords$shape,
                       gp = grid::gpar(#alpha = coords$alpha
                                       col = condition,
                                       lty = condition)
                     )
                     
                   }
)

# The geom_* function is constructed as a regular function. This function returns a layer 
# that can be added to a plot created with the ggplot() function.

geom_drop <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomDropout, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}