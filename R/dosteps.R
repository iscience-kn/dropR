dosteps <- function(x, y,return_df = T) {
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
