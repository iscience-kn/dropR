#' Plot Dropout Curves
#' 
#' This functions uses `ggplot2`to create drop out curves. 
#' Please note that you should use [add_dropout_idx()] and [compute_stats()] on your 
#' data before running this function as it needs a certain data structure and variables to 
#' work properly.
#' 
#' 
#' @param do_stats data.frame containing dropout statistics table computed by [compute_stats()].
#' Make sure your do_stats table contains a q_idx column indexing all question-items sequentially.
#' @param linetypes boolean Should different line types be used? Defaults to TRUE.
#' @param stroke_width numeric stroke width, defaults to 1.
#' @param full_scale boolean Should y axis range from 0 to 100? Defaults to TRUE, 
#' FALSE cuts off at min percent remaining (>0).
#' @param show_points boolean Should dropout curves show individual data points? Defaults to FALSE.
#' @param show_confbands boolean Should there be confidence bands added to the plot? Defaults to FALSE.
#' @param color_palette character indicating which color palette to use. Defaults to 'color_blind',
#' alternatively choose 'gray' or 'default' for the ggplot2 default colors. 
#' 
#' @import ggplot2
#' @importFrom grDevices gray
#' @importFrom stats sd
#' @export
#' 
#' @returns Returns a `ggplot` object containing the dropout curve plot. Using the Shiny App version of
#' dropR, this plot can easily be downloaded in different formats. 
#' 
#' @seealso [add_dropout_idx()] and [compute_stats()] which are necessary for the proper data structure.
#' 
#' @examples
#' do_stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' plot_do_curve(do_stats)
#' 
plot_do_curve <- function(do_stats,
                          linetypes = TRUE,
                          stroke_width = 1,
                          full_scale = TRUE,
                          show_points = FALSE,
                          show_confbands = FALSE,
                          color_palette = "color_blind"){
  # Resolve global variable issue
  q_idx <- pct_remain <- condition <- NULL
  
  do_curve <- ggplot(do_stats)
  
  palette <- if(color_palette == "color_blind"){c("#000000", "#E69F00",
                                                  "#56B4E9", "#009E73",
                                                  "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7")
  } else {gray(seq(from = 0,1,
                   by = 1/8)[c(1,8,3,7,4,5,2,6)])}
  
  if(linetypes){
    do_curve <- do_curve + geom_line(aes(x = q_idx,
                                         y = (pct_remain)*100,
                                         col = factor(condition),
                                         linetype = factor(condition)),
                                     linewidth = as.numeric(stroke_width))
  } else {
    do_curve <- do_curve + geom_line(aes(x=q_idx,
                                         y=(pct_remain)*100,
                                         col = factor(condition)),
                                     linewidth = as.numeric(stroke_width))  
  }
  do_curve <- do_curve + 
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) + 
    xlab("Question Index") +
    ylab("Percent Remaining")
  
  if(full_scale){
    do_curve <- do_curve + 
      scale_y_continuous(limits = c(0,100))
  }
  
  
  if(show_points){
    do_curve <- do_curve + geom_point(aes(x=q_idx,
                                          y=(pct_remain)*100,
                                          col=condition),
                                      size = as.numeric(stroke_width)*1.5)
  }
  
  if(show_confbands){
    do_curve <- do_curve + geom_ribbon(aes(x = q_idx, 
                                           ymin = pct_remain*100 - .5*sd(pct_remain*100),
                                           ymax = pct_remain*100 + .5*sd(pct_remain*100),
                                           fill = condition),
                                       alpha=.2)
    if(color_palette != "default"){
      do_curve <- do_curve + scale_fill_manual(values=palette)
    }
  }
  
  if(color_palette != "default" & length(levels(do_stats$condition) < 9)){
    do_curve <- do_curve + scale_color_manual(values=palette)
  }
  
  
  do_curve <- do_curve + guides(color = guide_legend(title = NULL),
                                linetype = guide_legend(NULL),
                                fill=guide_legend(title = NULL, position = NULL)) #relevant for confbands
  
  do_curve
}
