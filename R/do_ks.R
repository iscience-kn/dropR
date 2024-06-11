#' Compute Kolmogorov-Smirnov Test for most extreme conditions
#' 
#' This test is used for survival analysis between the most extreme conditions,
#' so the ones with the most different rates of dropout.
#' This function automatically prepares your data and runs `stats::ks.test()` on it.
#'
#' @param data A data frame made from [compute_stats()], containing information on the percent remaining per question per condition
#' @param question Index of question to be included in analysis, commonly the last question of the survey.
#'
#' @importFrom dplyr %>% filter pull
#' @importFrom stats ks.test
#' 
#' @returns result of Kolmogorov-Smirnoff test including which conditions have the most different dropout rates.
#' @export
#' 
#' @examples
#' stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_ks(stats, 52)
#' 
#' 
do_ks <- function(data, question){
  extremes <- data %>% 
    filter(q_idx == question,
           condition != "total") %>% 
    filter(pct_remain == max(pct_remain) | pct_remain == min(pct_remain))
  
  extremes <- extremes$condition[extremes$pct_remain %in% range.default(extremes$pct_remain)]
  
  res <- ks.test(x = stats$pct_remain[stats$condition == extremes[1]],
          y = stats$pct_remain[stats$condition == extremes[2]])
  
  res$method <- paste0(res$method, " of conditions ", extremes[1], " & ", extremes[2])
  
  res$extremes <- extremes
  
  res$data.name <- gsub("extremes\\[1\\]", res$extremes[1], res$data.name)
  res$data.name <- gsub("extremes\\[2\\]", res$extremes[2], res$data.name)
  
  res
}
