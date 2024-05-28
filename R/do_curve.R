#' Plot Dropout Curves
#' 
#' This functions uses ggplot to create drop out curves. 
#' Please note that you should use [add_dropout_idx()] and [compute_stats()] on your 
#' data before trying to run this function as it needs a certain data structure to 
#' work properly.
#' 
#' 
#' @param d data.frame containing dropout statistics table computed by [compute_stats()].
#' Make sure your stats table contains a do_idx column indexing all items sequentially.
#' @param linetypes boolean for use of different linetypes, defaults to TRUE.
#' @param stroke_width numeric stroke width, defaults to 1.
#' @param full_scale boolean should y axis range from 0 to 100? Defaults to TRUE, 
#' FALSE cuts off at max percent remaining.
#' @param show_points boolean should lines show points? Defaults to FALSE.
#' @param color_palette character indicating which color palette to use. Defaults to 'color_blind',
#' alternatively choose 'gray'. 
#' @import ggplot2
#' @export
#' 
#' @examples
#' stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' plot_do_curve(stats)
#' 
plot_do_curve <- function(d,
                          linetypes = TRUE,
                          stroke_width = 1,
                          full_scale = TRUE,
                          show_points = FALSE,
                          color_palette = "color_blind"
                          ){
  do_curve <- ggplot(d)
  
  if(linetypes){
    do_curve <- do_curve + geom_line(aes(x = do_idx,
                                         y = (pct_remain)*100,
                                         col = factor(condition),
                                         linetype = factor(condition)),
                                     size = as.numeric(stroke_width))
  } else {
    do_curve <- do_curve + geom_line(aes(x=drop_out_idx,
                                         y=(pct_remain)*100,
                                         col = factor(condition)),
                                     size = as.numeric(stroke_width))  
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
    do_curve <- do_curve + geom_point(aes(x=drop_out_idx,
                                          y=(pct_remain)*100,
                                          col=condition),
                                      size = as.numeric(input$stroke_width)*1.5)
  }
  
  if(color_palette == "color_blind" & length(levels(d$condition) < 9)){
    do_curve <- do_curve + scale_color_manual(values=c("#000000", "#E69F00",
                                                       "#56B4E9", "#009E73",
                                                       "#F0E442", "#0072B2",
                                                       "#D55E00", "#CC79A7"))
  }
  
  if(color_palette == "gray" & length(levels(d$condition) < 9)){
    do_curve <- do_curve + 
      scale_color_manual(values = gray(seq(from = 0,1,
                                           by = 1/8)[c(1,8,3,7,4,5,2,6)]
      )
      )
  }
  
  do_curve <- do_curve + guides(color = guide_legend(title = NULL),
                                linetype = guide_legend(NULL))
  
  do_curve
}
