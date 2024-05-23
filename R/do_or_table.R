#' Dropout Odds Ration Table
#' 
#' 
#' 
#' @param d data.frame stats table as computed by compute_stats.
#' @param chisq_question description
#' @param sel_cond_chisq description
#' @export
do_or_table <- function(d,
                        chisq_question,
                        sel_cond_chisq){
  d <- as.data.frame(stats())
  d <- subset(d,condition %in% sel_cond_chisq)
  d$condition <- factor(d$condition)
  # d <- subset(d,condition != "total")
  
  test_input <- subset(d, do_idx == chisq_question)
  
  OR_matrix <- outer(test_input$pct_remain,
                     test_input$pct_remain,
                     FUN = get_odds_ratio)
  colnames(OR_matrix) <- test_input$condition
  row.names(OR_matrix) <- test_input$condition
  OR_matrix
}
