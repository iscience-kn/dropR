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

