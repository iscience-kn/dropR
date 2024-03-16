#' Extract Drop Out from a Data.Frame
#' 
#' Find drop out posititions in a data.frame that contains multiple 
#' questions that had been asked sequentially.
#' 
#' @param df a data.frame
#' @param q_pos numeric columns that contain questions
#' @export
add_dropout_idx <- function(df, q_pos){
  nms <- names(df[,q_pos])
  tf <- is.na(df[,q_pos])
  tpos <- apply(tf, 1, which)
  tpos[lapply(tpos, length) == 0] <- NA
  # cover unit NR
  tpos[lapply(tpos, length) == length(q_pos)] <- 1

  # out vector contains drop out position
  # dpos <- 
  df$do_idx <- sapply(tpos, find_drop_out, clnms = nms)
  df
}

