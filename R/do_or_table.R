#' Dropout Odds Ratio Table
#' 
#' 
#' 
#' @param df data.frame stats table as computed by compute_stats.
#' @param chisq_question description
#' @param sel_cond_chisq description
#' @export
do_or_table <- function(df,
                        chisq_question,
                        sel_cond_chisq){
  df <- as.data.frame(df)
  df <- subset(df,condition %in% sel_cond_chisq)
  df$condition <- factor(df$condition)
  # d <- subset(d,condition != "total")
  
  test_input <- subset(df, q_idx == chisq_question)
  
  OR_matrix <- outer(test_input$pct_remain,
                     test_input$pct_remain,
                     FUN = get_odds_ratio)
  colnames(OR_matrix) <- test_input$condition
  row.names(OR_matrix) <- test_input$condition
  OR_matrix
}
