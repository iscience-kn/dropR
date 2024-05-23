#' Compute Kolmogorov-Smirnov Test for most extreme conditions
#'
#' @param data A dataset made from dropR::compute_stats(), containing information on the percent remaining per question per condition
#' @param questions Number of questions to be included in analysis
#'
#' @importFrom dplyr %>% filter pull
#' @importFrom stats ks.test
#' @export
do_ks <- function(data, questions){
  
  extremes <- data %>% 
    filter(do_idx == questions, #length(input$quest_cols)-1
           condition != "total") %>% 
    filter(pct_remain == max(pct_remain) | pct_remain == min(pct_remain)) %>% 
    pull(condition)
  
  # extreme_data <- stats %>% 
  #   filter(condition %in% extremes)
  # stats$pct_remain[stats$condition == extremes[1]]
  
  ks.test(x = stats$pct_remain[stats$condition == extremes[1]],
          y = stats$pct_remain[stats$condition == extremes[2]])
  
}
