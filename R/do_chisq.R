#' Compute Chisq-Test Given a Question Position
#' 
#' This function performs a chi-squared contingency table test on dropout for
#' a given question in the data. Note that the input data should be in the format as 
#' computed by [compute_stats()]. 
#' The test can be performed on either all conditions (excluding total) or on select conditions.
#' 
#' @param d data.frame stats table computed by [compute_stats()].
#' @param chisq_question numeric question.
#' @param sel_cond_chisq vector (same class as in conditions variable in original dataset) selected conditions.
#' @param p_sim boolean simulate p value parameter by Monte Carlo simulation.
#' @export
#' 
#' @examples
#' stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_chisq(stats, 47, c(12, 22), T)
#' 
do_chisq <- function(d,
                     chisq_question,
                     sel_cond_chisq,
                     p_sim){
  d <- subset(d, condition %in% sel_cond_chisq)
  d$condition <- factor(d$condition)
  
  # d <- subset(d,condition != "total")
  
  test_input <- subset(d, do_idx == chisq_question)
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
