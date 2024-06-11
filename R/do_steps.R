#' Calculate Steps for Uneven Data Points
#'
#' The `do_steps` function calculates steps for data points represented by numbers of questions from the original
#' experimental or survey data in `x` and remaining percent of participants in `y`.
#' 
#' Due to the nature of dropout/ survival data, step functions are necessary to accurately depict participants remaining.
#' Dropout data includes the time until the event (a.k.a. dropout at a certain question or time), so that changes in remaining
#' participants are discrete rather than continuous. This means that changes in survival probability occur at specific points 
#' and are better represented as steps than as a continuum.
#'
#' @param x Numeric vector representing the question numbers
#' @param y Numeric vector representing the remaining percent of participants
#' @param return_df Logical. If `TRUE`, the function returns a data frame; otherwise, it returns a list.
#'
#' @return A data frame or a list containing the modified `x` and `y` values
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(100, 100, 95, 90, 85)
#' do_steps(x, y)
#'
#' # Using the example dataset dropRdemo
#' 
#' stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' tot_stats <- stats[stats$condition == "total", ]
#' do_steps(tot_stats$q_idx, tot_stats$pct_remain)
#'
#' @export
do_steps <- function(x, y, return_df = T) {
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) 
    return()
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  
  n <- length(x)
  if (n == 1) 
    list(x = x, y = y)
  else if (n == 2) 
    list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)])
  else {
    temp <- rle(y)$lengths
    drops <- 1 + cumsum(temp[-length(temp)])
    if (n %in% drops) {
      xrep <- c(x[1], rep(x[drops], each = 2))
      yrep <- rep(y[c(1, drops)], c(rep(2, length(drops)), 
                                    1))
    }
    else {
      xrep <- c(x[1], rep(x[drops], each = 2), x[n])
      yrep <- c(rep(y[c(1, drops)], each = 2))
    }
    if(return_df){
      as.data.frame(list(x = xrep, y = yrep))
    } else {
      list(x = xrep, y = yrep)  
    }
  }
}
