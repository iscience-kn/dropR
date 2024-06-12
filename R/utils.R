#' Occurrence Count
#' 
#' The `n_count` function calculates the number of occurrences of a specific 
#' value `n` in a numeric vector `x`.
#' 
#' `r lifecycle::badge("questioning")`
#'
#' @param x Vector in which to count occurrences.
#' @param n Value to count occurrences of.
#'
#' @returns An integer representing the count of occurrences of `n` in `x`.
#' 
#' @source R/utils.R
#'
#' @examples
#' x <- c(1, 2, 3, 2, 4, 2)
#' n_count(x, 2)  
#' # Should return 3
#' 
#' n_count(c("A", "B", "B"), "B")
#' # Should return 2
#' 
#' n_count(c(TRUE, TRUE, FALSE), FALSE)
#' # Should return 1
#' 
#' n_count(c("A", "B", "B"), "b")
#'# Should return 0 as it is not an exact match (case sensitive)
#'
#' @export
n_count <- function(x, n){length((which(x == n)))}

#' Concatenate Names and Values of a Named Vector
#'
#' `r lifecycle::badge("questioning")`
#' The `all_values` function takes a named vector `x` and concatenates the names 
#' and formatted values into a single string which is separated by HTML <br> tags.
#'
#' @param x Named vector for which to concatenate names and values.
#'
#' @returns A character vector containing the names and formatted values of the input vector, separated by line breaks.
#'
#' @source R/utils.R
#' 
#' @examples
#' my_vector <- c(apples = 5, bananas = 3, oranges = 2)
#' all_values(my_vector)
#'
#' @export
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

#' Get Steps Data by Condition
#' 
#' @source R/utils.R
#'
#' The `get_steps_by_cond` function calculates steps data based on survival model results. 
#' This utility function is used inside the [do_kpm()] function of `dropR`.
#'
#' @param sfit An object representing survival model results (e.g., from a Kaplan-Meier model).
#' @param condition Optional. An experiemtnal condition to include in the output data frame, defaults to `NULL`.
#' 
#' @seealso [do_kpm()]
#'
#' @return A data frame containing the steps data, including time, survival estimates, upper confidence bounds, and lower confidence bounds.
#'
#' @export
get_steps_by_cond <- function(sfit, condition = NULL){
  f <- do_steps(sfit$time,sfit$surv)
  u <- do_steps(sfit$time,sfit$upper)
  l <- do_steps(sfit$time,sfit$lower)
  dframe <- cbind(f,uppr = u$y, lwr = l$y)
  if(!is.null(condition)) dframe$condition <- condition
  dframe
}
