#' Compute Odds From Probabilities
#' 
#' Compute odds from probabilities. The function is vectorized and
#' can handle a vector of probabilities, e.g. remaining percent of participants
#' as calculated by [compute_stats()]. 
#' 
#' @param p vector of probabilities. May not be larger than 1 or smaller than zero.
#' @export
#' 
#' @returns Returns numerical vector of the same length as original input reflecting the odds.
#' 
#' @examples
#' get_odds(0.7)
#' @examples
#' get_odds(c(0.7, 0.2))
#' 
#' 
get_odds <- function(p){
  if(!all(p <= 1 & p >= 0)) stop('Input is not a probability!')
  p / (1-p)
} 


#' Compute Odds Ratio 
#' 
#' Computes odds ratio given two probabilities.
#' In this package, the function can be used to compare the percentages of remaining
#' participants between two conditions at a time.
#' 
#' 
#' @param a numeric probability value between 0 and 1.
#' @param b numeric probability value between 0 and 1.
#' @export
#' 
#' @seealso [get_odds()], as this is the basis for calculation.
#' 
#' @returns Returns numerical vector of the same length as original input reflecting the Odds Ratio (OR).
#' 
#' @examples
#' get_odds_ratio(0.7, 0.6)
#' 
get_odds_ratio <- function(a,b){
  get_odds(a)/get_odds(b)
}


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
#' @param return_df Logical. If TRUE, the function returns a data frame; otherwise, it returns a list.
#'
#' @returns Returns a data frame or a list containing the modified `x` and `y` values.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(100, 100, 95, 90, 85)
#' do_steps(x, y)
#'
#' # Using the example dataset dropRdemo
#' 
#' do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' tot_stats <- do_stats[do_stats$condition == "total", ]
#' do_steps(tot_stats$q_idx, tot_stats$pct_remain)
#'
#' @export
do_steps <- function(x, 
                     y, 
                     return_df = TRUE){
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) { return()}
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  
  n <- length(x)
  if (n == 1) {
    list(x = x, y = y) 
  } else if (n == 2) {
    list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)]) 
  } else {
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
