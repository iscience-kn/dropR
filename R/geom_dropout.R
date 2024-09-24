# The ggproto() function is used to construct a new class corresponding to your new geom. 
# This new class specifies a number of attributes and functions that describe how data should be drawn on a plot.
# 
# library(grid)
# GeomDropout <- ggproto("GeomDropout", Geom,
#                    # required_aes = c("x", "y", "condition"), # such as x and y
#                    default_aes = aes(linewidth = 1, color = "green"), # <default values for certain aesthetics>, plot symbol, color
#                    draw_key = draw_key_abline, # <a function used to draw the key in the legend>
#                    draw_panel = function(data, panel_scales, coord) {
#                      ## Function that returns a grid grob that will 
#                      ## be plotted (this is where the real work occurs)
#                 
#                      
#                      # Add the correct variables
#                      # x <- data$q_idx
#                      # 
#                      # y <- data$pct_remain*100
#                      # 
#                      # condition <- factor(data$condition)
#                      
#                      ## Transform the data to match the coordinate system
#                      coords <- coord$transform(data, panel_scales)
#                      
#                      # needs some knowledge of the grid package
#                      # The data element is a data frame containing one column for each aesthetic specified, 
#                      # panel_scales is a list containing information about the x and y scales for the current panel, and 
#                      # coord is an object that describes the coordinate system of your plot.
#                      
#                      ## Construct a grid grob
#                      grid::linesGrob(
#                        x = coords$x,
#                        y = coords$y
#                        # ,
#                        # # pch = coords$shape,
#                        # gp = grid::gpar(#alpha = coords$alpha
#                        #                # col = coords$condition,
#                        #                 lty = "dashed") # coords$condition
#                      )
#                      
#                    }
# )

# GeomDropout <- ggproto("GeomDropout", GeomLine,
#                        # default_aes = aes(linewidth = 1)
#                        
#                        default_aes = aes( # define a default color
#                          linewidth = 1
#                          # alpha = 1,
#                          # linetype = 1 # include alpha to handle transparency
#                        ),
#                        
#                        # Ensure it handles colors correctly by drawing the panel with all parameters
#                        draw_panel = function(data, panel_params, coord) {
#                          # Transform the data as needed
#                          coords <- coord$transform(data, panel_params)
#                          
#                          # Use grid::linesGrob to draw lines and pass color and alpha
#                          grid::linesGrob(
#                            coords$x, coords$y,
#                            
#                            gp = grid::gpar(col = coords$colour,
#                              # col = alpha(coords$colour, coords$alpha),
#                              # lwd = coords$linewidth,
#                              lty = coords$linetype
#                            )
#                          )
#                        }
#                        
#                        
#                        )
                       
# The geom_* function is constructed as a regular function. This function returns a layer 
# that can be added to a plot created with the ggplot() function.

geom_drop <- function(mapping = aes(x = q_idx, y = pct_remain*100, 
                                    color = factor(condition), linetype = factor(condition)), 
                      data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, 
                      show.legend = NA, inherit.aes = FALSE, ...) {
  ggplot2::layer(
    geom = "line", mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, linewidth = 1, ...)
  ) 
}


ggplot(do_stats) +
  geom_drop() +
  dropR_style(palette = 1)


plot_do_curve(do_stats, full_scale = F)

ggplot(do_stats, aes(x = q_idx, y = pct_remain*100, color = factor(condition))) +
  layer(
     geom = "line", stat = "identity", position = "identity"
  )


  



theme_drop <- function(){

    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) 

}




# gray_seq <- function(nums){
#   # n <- length(nums)
#   # 
#   # seqs <- seq(0,1, by = 1/n)
#   
#   gray(seq(0, 1, 
#            by = 1/length(nums)))[c(seq(2, length(nums), by = 2), # odd
#                                    seq(1, length(nums), by = 2))] # even
#   
#   # even <- seq(2, n, by=2)
#   # odd <- seq(1, n, by=2)
#   
#   # return(gray(seqs)[c(seq(2, n, by=2), seq(1, n, by=2))])
# }
# num <- 1:7
# gray_seq(num) |> show_col()

dropR_style <- function(palette = 1,
                        n_cond = 5,
                        xlab = "Question", 
                        ylab = "Percent Remaining",
                        colorlab = "Condition"){
  # # Gray scale function
  # gray_seq <- function(nums){
  #   gray(seq(0, 1, 
  #            by = 1/length(nums)))[c(seq(2, length(nums), by = 2), # odd
  #                                    seq(1, length(nums), by = 2))] # even
  # }
  
  # Palette definition
  paletteses <- list(
    color_blind = c("#000000", "#E69F00",
                    "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2",
                    "#D55E00", "#CC79A7"), 
    gray = gray(seq(0, 1, 
                    by = 1/n_cond))[c(seq(2, n_cond, by = 2), # odd
                                            seq(1, n_cond, by = 2))], # even,
    ggdefault = c("#F8766D", "#D39200", "#93AA00",
                  "#C77CFF", "#00C19F", "#00B9E3",
                  "#E68613", "#00A9FF", "#FF61C3"),
    wextor = c("#326699", "#53D8FB", "#66C3FF", "#B79FAD", "#B3424F") #870058-c97f54-ffeccc-c8d6af-104547-ed1c24
  )
  
  # chosen_pal <- paletteses[[palette]]
  
  style <- list(
    theme_drop(),
    scale_colour_manual(values = paletteses[[palette]]),
    labs(x = xlab, y = ylab, color = colorlab, linetype = colorlab)
  )
  
  return(style)
}
