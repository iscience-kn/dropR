#' Extract Drop Out from a Data.Frame
#' 
#' Find drop out positions in a data.frame that contains multiple 
#' questions that had been asked sequentially.
#' 
#' @param df a data.frame
#' @param q_pos numeric columns that contain questions
#' @export
add_dropout_idx <- function(df, q_pos){
  df$do_idx <- rowSums(sapply(df[, q_pos], is.na))
  
  # identify single missing by checking whether the NAs are in sequence (one missing is NOT a sequence)
  is.sequential <- function(x){
    ifelse(length(x) == 1, F, all(diff(x) == 1)) # all(abs(diff(x)) == 1)
  } # Not sequential - not dropout.
  
  for(i in 1:nrow(df)){
    x <- which(is.na(df[i, ]))
    
    if(!is.sequential(x)){
      df$do_idx[i] <- 0 # not sequential - not dropouot
    } else if(!df$do_idx[i] %in% c(0, length(q_pos))){
      df$do_idx[i] <- length(q_pos) - df$do_idx[i] # if dropout, it should give us the correct dropout position, not the number of NAs
    }
  }
  
  df
}

