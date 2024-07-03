#' Get Steps Data by Condition
#' 
#' The `get_steps_by_cond` function calculates steps data based on survival model results. 
#' This utility function is used inside the [do_kpm()] function of `dropR`.
#'
#' @param sfit An object representing survival model results (e.g., from a Kaplan-Meier model).
#' @param condition Optional. An experimental condition to include in the output data frame, defaults to `NULL`.
#' 
#' @seealso [do_kpm()]
#'
#' @returns Returns a data frame containing the steps data, including time, survival estimates, upper confidence bounds, and lower confidence bounds.
#'
#' @export
get_steps_by_cond <- function(sfit, 
                              condition = NULL){
  f <- do_steps(sfit$time,sfit$surv)
  u <- do_steps(sfit$time,sfit$upper)
  l <- do_steps(sfit$time,sfit$lower)
  dframe <- cbind(f,uppr = u$y, lwr = l$y)
  if(!is.null(condition)) dframe$condition <- condition
  dframe
}
