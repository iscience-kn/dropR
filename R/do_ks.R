#' Compute Kolmogorov-Smirnov Test for most extreme conditions
#' 
#' This test is used for survival analysis between the most extreme conditions,
#' so the ones with the most different rates of dropout.
#' This function automatically prepares your data and runs `stats::ks.test()` on it.
#'
#' @param do_stats A data frame made from [compute_stats()], containing information on the percent remaining per question per condition
#' @param question Index of question to be included in analysis, commonly the last question of the survey.
#'
#' @importFrom stats ks.test
#' 
#' @returns Returns result of Kolmogorov-Smirnoff test including which conditions have the most different dropout rates.
#' @export
#' 
#' @examples
#' do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_ks(do_stats, 52)
#' 
#' 
do_ks <- function(do_stats, 
                  question){
  
  extremes <- do_stats[do_stats$q_idx == question & do_stats$condition != "total",]
  extremes <- extremes$condition[extremes$pct_remain == max(extremes$pct_remain) | 
                                 extremes$pct_remain == min(extremes$pct_remain)]
  
  res <- ks.test(x = do_stats$pct_remain[do_stats$condition == extremes[1]],
          y = do_stats$pct_remain[do_stats$condition == extremes[2]])
  
  res$method <- paste0(res$method, " of conditions ", extremes[1], " & ", extremes[2], " at question ", question)
  
  res$extremes <- extremes
  
  res$data.name <- gsub("extremes\\[1\\]", res$extremes[1], res$data.name)
  res$data.name <- gsub("extremes\\[2\\]", res$extremes[2], res$data.name)
  
  res
}






#' Plot Most Extreme Conditions to Visualize Kolmogorov-Smirnov Test Results
#' 
#' With this function, you can easily plot the most extreme conditions, a.k.a. those with the most
#' different dropout rates at a certain question. You need to define that question in the function call of
#' [do_ks()] already, or just call that function directly inside the plot function.
#'
#' @param do_stats data.frame containing dropout statistics table computed by [compute_stats()].
#' Make sure your do_stats table contains a q_idx column indexing all question-items sequentially.
#' @param ks List of results from the [do_ks()] function coding most extreme dropout conditions
#' @param linetypes boolean Should different line types be used? Defaults to FALSE. 
#' @param show_confbands boolean Should there be confidence bands added to the plot? Defaults to FALSE.
#' @param color_palette character indicating which color palette to use. Defaults to color blind friendly values,
#' alternatively choose 'gray' or create your own palette with two colors, e.g. using R [colors()] or HEX-values 
#'
#' @import ggplot2
#' @importFrom grDevices gray
#' @importFrom stats sd
#' @returns Returns a `ggplot` object containing the survival curve plot of the most extreme
#' dropout conditions. Using the Shiny App version of dropR, this plot can easily be downloaded in different formats. 
#' @export
#' 
#' @seealso [compute_stats()], [do_ks()]
#'
#' @examples
#' do_stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54), 
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' ks <- do_ks(do_stats, 52)
#' 
#' plot_do_ks(do_stats, ks, color_palette = "gray")
#' 
#' # ... or call the do_ks() function directly inside the plotting function
#' plot_do_ks(do_stats, do_ks(do_stats, 30))
#' 
#' plot_do_ks(do_stats, ks, linetypes = TRUE, 
#' show_confbands = TRUE, color_palette = c("red", "violet"))
#' 
plot_do_ks <- function(do_stats,
                       ks,
                       linetypes = FALSE,
                       show_confbands = FALSE,
                       color_palette = c("#E69F00", "#CC79A7")){
  
  ks_steps1 <- do_steps(do_stats$q_idx[do_stats$condition == ks$extremes[1]],
                        do_stats$pct_remain[do_stats$condition == ks$extremes[1]])
  
  ks_steps2 <- do_steps(do_stats$q_idx[do_stats$condition == ks$extremes[2]],
                        do_stats$pct_remain[do_stats$condition == ks$extremes[2]])
  
  
  palette <- if(length(color_palette) > 1){color_palette} # users can supply their own colors
  else if (color_palette == "gray"){gray(seq(from = 0,1,
                   by = 1/8)[c(2,6)])}
  else {c("#E69F00", "#CC79A7")}
  
  
  ks_plot <- ggplot() +
    geom_line(aes(ks_steps1$x, ks_steps1$y*100, color = as.character(ks$extremes[1])), linetype = ifelse(linetypes, 3, 1)) +
    geom_line(aes(ks_steps2$x, ks_steps2$y*100, color = as.character(ks$extremes[2])), linetype = ifelse(linetypes, 5, 1)) +
    scale_color_manual(name = "Conditions", 
                       values = c(palette[1], palette[2])) +
    scale_x_continuous(breaks = function(x) {
      pretty(x)[pretty(x) %% 1 == 0]
    })
  
  
  if(show_confbands){
    ks_plot <- ks_plot + geom_ribbon(aes(x = do_stats$q_idx[do_stats$condition == ks$extremes[1]], 
                                         ymin = do_stats$pct_remain[do_stats$condition == ks$extremes[1]]*100 - 
                                           .5*sd(do_stats$pct_remain[do_stats$condition == ks$extremes[1]]*100),
                                         ymax = do_stats$pct_remain[do_stats$condition == ks$extremes[1]]*100 + 
                                           .5*sd(do_stats$pct_remain[do_stats$condition == ks$extremes[1]]*100)),
                                     fill = palette[1], alpha=.2) + 
                         geom_ribbon(aes(x = do_stats$q_idx[do_stats$condition == ks$extremes[2]], 
                                           ymin = do_stats$pct_remain[do_stats$condition == ks$extremes[2]]*100 - 
                                             .5*sd(do_stats$pct_remain[do_stats$condition == ks$extremes[2]]*100),
                                           ymax = do_stats$pct_remain[do_stats$condition == ks$extremes[2]]*100 + 
                                             .5*sd(do_stats$pct_remain[do_stats$condition == ks$extremes[2]]*100)),
                                       fill = palette[2], alpha=.2)
    }
  
  ks_plot + 
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) + 
    xlab("Item Index") +
    ylab("Survival in %")
}

