#' Find the Position of Drop Out in a Vector
#' 
#' Check consecutive NAs from backend of a vector.
#' 
#' `r lifecycle::badge("deprecated")`
#'
#' `find_drop_out()` is deprecated in favor of [add_dropout_idx()], which is the 
#' basis for dropout analysis using `dropR`.
#' 
#' For reliable dropout analysis, please use [add_dropout_idx()] instead.
#' 
#' @param v a vector
#' @param clnms character. Columns of question items in the vector that actually hold questions.
#' @export
find_drop_out <- function(v,clnms){
  lifecycle::deprecate_warn("1.0", "find_drop_out()", "add_dropout_idx()")
  # length of the vector of question labels
  l <- length(clnms)
  # create backwards sequence
  full_seq <- l:1
  drop_out_pos <- min(which(!full_seq %in% v))
  l-(drop_out_pos-1)  
}
