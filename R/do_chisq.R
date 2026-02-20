#' Compute Chi-Squared Test Given a Question Position
#' 
#' This function performs a chi-squared contingency table test on dropout for
#' a given question in the data. Note that the input data should be in the format as 
#' computed by [compute_stats()]. 
#' The test can be performed on either all conditions (excluding total) or on select conditions.
#' 
#' @param do_stats data.frame of dropout statistics as computed by [compute_stats()].
#' @param chisq_question numeric Which question to compare dropout at.
#' @param sel_cond_chisq vector (same class as in conditions variable in original data set) selected conditions.
#' @param p_sim boolean Simulate p value parameter (by Monte Carlo simulation)? Defaults to `TRUE`.
#' 
#' @importFrom stats chisq.test
#' @export
#' 
#' @seealso [add_dropout_idx()] and [compute_stats()] which are necessary for the proper data structure.
#' 
#' @returns Returns test results from chisq.test between experimental conditions at defined question.
#' 
#' @examples
#' do_stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_chisq(do_stats, 47, c(12, 22), TRUE)
#' 
do_chisq <- function(do_stats,
                     chisq_question = max(unique(do_stats$q_idx)),
                     sel_cond_chisq = NULL,
                     p_sim = TRUE){
  # Resolve global variable issue
  q_idx <- condition <- NULL
  
  # If no conditions are selected, all but the total will be used
  if(is.null(sel_cond_chisq)){
    d <- subset(do_stats, condition != "total")
  } else{
    d <- subset(do_stats, condition %in% sel_cond_chisq)
  }
  
  d$condition <- factor(d$condition)
  
  test_input <- subset(d, q_idx == chisq_question)
  test_table <- as.table(as.matrix(test_input[,c("cs","remain")]))
  dimnames(test_table) <- list(conditions = test_input$condition,
                               participants = c("dropout","remaining"))
  # chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
  test_result <- chisq.test(test_table, simulate.p.value = p_sim)
  
  lname2 <- sprintf("Dropout up to item %s", chisq_question)
  li <- list("Test result" = test_result,
             lname2 = test_table)
  names(li)[2] <- lname2
  
  # Return
  structure(li,
            class = c("do_chi", "list"))
}
