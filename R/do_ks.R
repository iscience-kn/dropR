#' Compute Kolmogorov-Smirnov Test for most extreme conditions
#' 
#' This test is used for survival analysis between the most extreme conditions,
#' so the ones with the most different rates of dropout.
#' This function automatically prepares your data to run `stats::ks.test()` on it.
#'
#' @param data A data frame made from [compute_stats()], containing information on the percent remaining per question per condition
#' @param questions Number of questions to be included in analysis
#'
#' @importFrom dplyr %>% filter pull
#' @importFrom stats ks.test
#' 
#' @returns result of Kolmogorov-Smirnoff test
#' @export
do_ks <- function(data, questions){
  
  extremes <- data %>% 
    filter(do_idx == questions, #length(input$quest_cols)-1
           condition != "total") %>% 
    filter(pct_remain == max(pct_remain) | pct_remain == min(pct_remain))
  
  extremes <- extremes$condition[extremes$pct_remain %in% range.default(extremes$pct_remain)]
  
  ks.test(x = stats$pct_remain[stats$condition == extremes[1]],
                   y = stats$pct_remain[stats$condition == extremes[2]])
  
}
