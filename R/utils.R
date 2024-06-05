# ! Utils helper documentation is AI generated. 


#' The `n_count` function calculates the number of occurrences of a specific value `n` in a numeric vector `x`.
#'
#' @param x Numeric vector in which to count occurrences.
#' @param n Numeric value to count occurrences of.
#'
#' @return An integer representing the count of occurrences of `n` in `x`.
#'
#' @examples
#' x <- c(1, 2, 3, 2, 4, 2)
#' n_count(x, 2)  # Returns 3
#'
#' @export
n_count <- function(x, n){ length((which(x == n)))}

#' Concatenate Names and Values of a Named Vector
#'
#' The `all_values` function takes a named vector `x` and concatenates the names 
#' and formatted values into a single string which is separated by HTML <br> tags.
#'
#' @param x Named vector for which to concatenate names and values.
#'
#' @return A character vector containing the names and formatted values of the input vector, separated by line breaks.
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
#' The `get_steps_by_cond` function calculates steps data based on survival model results.
#'
#' @param sfit An object representing survival model results (e.g., from a Kaplan-Meier model).
#' @param condition Optional. A condition or group label to include in the output data frame.
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
