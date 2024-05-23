#' Extract Drop Out from a Data.Frame
#' 
#' Find drop out positions in a data.frame that contains multiple 
#' questions that had been asked sequentially.
#' 
#' @param df a data.frame
#' @param q_pos numeric columns that contain questions
#' @export
add_dropout_idx <- function(df, q_pos){
  # nms <- names(df[,q_pos])
  # tf <- is.na(df[,q_pos])
  # tpos <- apply(tf, 1, which)
  # tpos[lapply(tpos, length) == 0] <- NA
  # # cover unit NR
  # tpos[lapply(tpos, length) == length(q_pos)] <- 1
  # 
  # # out vector contains drop out position
  # # dpos <-
  # df$do_idx <- sapply(tpos, find_drop_out, clnms = nms)
  df$do_idx <- length(q_pos) - rowSums(sapply(df[, q_pos], is.na)) # length(q_pos) - 
  
  # identify single missing by using block entropy
  ent <- NA
  for(i in 1:nrow(df)){
    x <- q_pos - which(is.na(df[i, ]))
    
    p_xi <- table(x) / length(x)
    result <- sum(p_xi * log2(p_xi)) * (-1)
    
    ent[i] <- result
  }
  
  # df$do_idx[which(ent > 1)] <- 0 # length(q_pos)
  # df$do_idx[df$do_idx == length(q_pos)] <- 0
  df$do_idx[which(df$do_idx == length(q_pos) | ent > 1)] <- 0
  
  df
}

