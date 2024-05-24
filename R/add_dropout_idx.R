#' Extract Drop Out from a Data.Frame
#' 
#' Find drop out positions in a data.frame that contains multiple 
#' questions that had been asked sequentially.
#' 
#' @param df a data.frame
#' @param q_pos numeric columns that contain questions
#' @export
add_dropout_idx <- function(df, q_pos){
  foo <- df[rev(q_pos)]
  do <- NA
  for(line in 1:nrow(foo)){
    if(is.na(foo[line, 1])){ # if there is an NA at col 1 (last question col of original data), probably dropout
      do[line] <- 1
      for(col in 2:ncol(foo)){
        if(is.na(foo[line, col])){
          do[line] <- do[line] + 1
        } else {
          next
        }
      }
    } else{
      do[line] <- 0
      next
    }
    # browser()
  }
  df$do_idx <- ifelse(do == 0, 0, 
                      ifelse(do == length(q_pos), length(q_pos),
                             length(q_pos) - do)) # 0 stays 0, if all are empty that stays as well
  df
}

