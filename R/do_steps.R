# Caution: AI Generated Documentation here... 
#' Calculate Steps for Uneven Data Points
#'
#' The `do_steps` function calculates steps for uneven data points represented by vectors `x` and `y`.
#'
#' @param x Numeric vector representing the x-coordinates.
#' @param y Numeric vector representing the y-coordinates.
#' @param return_df Logical. If `TRUE`, the function returns a data frame; otherwise, it returns a list.
#'
#' @return A data frame or a list containing the modified `x` and `y` coordinates.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(10, 20, 30, 40, 50)
#' do_steps(x, y)
#'
#' @export
do_steps <- function(x, y,return_df = T, na.rm = T) {
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) 
    return()
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  # 
  # if (!na.rm){ # if there are missings in the data and we don't want to just drop them, we imputate the last known value
  #   x[is.na(x)] <- x[max(rle(is.na(x))$lengths)]
  #   y[is.na(y)] <- y[max(rle(is.na(y))$lengths)]
  # }
  
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
