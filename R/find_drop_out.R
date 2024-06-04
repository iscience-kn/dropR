#' Find the Position of Drop Out in a Vector
#' 
#' Check consecutive NAs from backend of a vector. 
#' `r lifecycle::badge("deprecated")`
#'
#' `find_drop_out()` is deprecated in favor of [add_dropout_idx()], which is the 
#' basis for dropout analysis using `dropR`.
#' 
#' @param v a vector
#' @param clnms specify the parts that actually hold questions by character names.
#' @export
find_drop_out <- function(v,clnms){
  # length of the vector of question labels
  l <- length(clnms)
  # create backwards sequence
  full_seq <- l:1
  drop_out_pos <- min(which(!full_seq %in% v))
  l-(drop_out_pos-1)  
}
