#' Compute Chisq-Test Given a Question Position
#' 
#' 
#' 
#' @param d data.frame stats table computed by compute_stats.
#' @param chisq_question question.
#' @param sel_cond_chisq selected condition.
#' @param fisher simulate p value parameter.
#' @export
do_chisq <- function(d,
                     chisq_question,
                     sel_cond_chisq,
                     fisher){
  d <- subset(d, condition %in% sel_cond_chisq)
  d$condition <- factor(d$condition)
  
  # d <- subset(d,condition != "total")
  
  test_input <- subset(d, do_idx == chisq_question)
  test_table <- as.table(as.matrix(test_input[,c("cs","remain")]))
  dimnames(test_table) <- list(conditions = test_input$condition,
                               participants = c("dropout","remaining"))
  # chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
  test_result <- chisq.test(test_table, simulate.p.value = fisher)
  lname2 <- sprintf("Dropout at question %s", chisq_question)
  li <- list("Test result" = test_result,
             lname2 = test_table)
  names(li)[2] <- lname2
  li
}
