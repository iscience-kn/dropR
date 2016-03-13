n_count <- function(x, n){ length((which(x == n)))}


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
