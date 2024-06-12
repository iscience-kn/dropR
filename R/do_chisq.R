#' Compute Chisq-Test Given a Question Position
#' 
#' This function performs a chi-squared contingency table test on dropout for
#' a given question in the data. Note that the input data should be in the format as 
#' computed by [compute_stats()]. 
#' The test can be performed on either all conditions (excluding total) or on select conditions.
#' 
#' @param df data.frame of stats as computed by [compute_stats()].
#' @param chisq_question numeric Which question to compare dropout at.
#' @param sel_cond_chisq vector (same class as in conditions variable in original data set) selected conditions.
#' @param p_sim boolean Simulate p value parameter (by Monte Carlo simulation)? Defaults to `TRUE`.
#' 
#' @importFrom stats chisq.test
#' @export
#' 
#' @seealso [add_dropout_idx()] and [compute_stats()] which are necessary for the proper data structure.
#' 
#' @examples
#' stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_chisq(stats, 47, c(12, 22), TRUE)
#' 
do_chisq <- function(df,
                     chisq_question,
                     sel_cond_chisq,
                     p_sim = TRUE){
  # Resolve global variable issue
  q_idx <- condition <- NULL
  
  d <- subset(df, condition %in% sel_cond_chisq)
  d$condition <- factor(d$condition)
  
  # d <- subset(d,condition != "total")
  
  test_input <- subset(d, q_idx == chisq_question)
  test_table <- as.table(as.matrix(test_input[,c("cs","remain")]))
  dimnames(test_table) <- list(conditions = test_input$condition,
                               participants = c("dropout","remaining"))
  # chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
  test_result <- chisq.test(test_table, simulate.p.value = p_sim)
  lname2 <- sprintf("Dropout at question %s", chisq_question)
  li <- list("Test result" = test_result,
             lname2 = test_table)
  names(li)[2] <- lname2
  li
}
