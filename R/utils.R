n_count <- function(x, n){ length((which(x == n)))}


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}


getStepsByCond <- function(sfit,condition = NULL){
  f <- dosteps(sfit$time,sfit$surv)
  u <- dosteps(sfit$time,sfit$upper)
  l <- dosteps(sfit$time,sfit$lower)
  dframe <- cbind(f,uppr = u$y, lwr = l$y)
  if(!is.null(condition)) dframe$condition <- condition
  dframe
}
