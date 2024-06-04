#' Add Dropout Index to a Data.Frame
#' 
#' Find drop out positions in a data.frame that contains multiple 
#' questions that had been asked sequentially.
#' This function adds the Dropout Index variable "do_idx" to the data.frame which is necessary
#' for further analyses of dropout.
#' 
#' # Usage
#' 
#' Use this function _first_ to prepare your dropout analysis. Then, keep going by creating
#' the dropout statistics using [compute_stats()].
#' Importantly, this function will start counting missing data starting
#' at the end of the data frame. Any missing data which is somewhere in between, i.e.
#' a single question that was skipped or forgotten will not be counted as dropout.
#' The function will search for sequences of missing data that go until the end of the 
#' data frame and count those.
#' 
#' Therefore, the variables must be in the order that they were asked, otherwise analyses
#' will not be reliable.
#' 
#' @param df a data.frame
#' @param q_pos numeric columns that contain questions
#'
#' @export
#' 
#' @returns Original data frame with column `do_idx` added.
#' 
#' @examples
#' dropout <- add_dropout_idx(dropR::dropRdemo, 3:54)
#' 
add_dropout_idx <- function(df, q_pos){
  # Create a reversed logical matrix where TRUE indicates NA 
  na_matrix <- is.na(df[, rev(q_pos), drop = FALSE])
  
  # Identify rows where the last column is NA (aka candidates for dropout)
  last_col_na <- na_matrix[, 1]
  
  # For rows with last column NA, find the first non-NA going backwards
  na_cumsum <- t(apply(na_matrix, 1, cumsum))
  na_cumsum[!last_col_na, ] <- 0
  # rowmax <- apply(na_cumsum, 1, max)
  
  # Find plateau in cumsums (aka beginning of dropout)
  difmat <- t(apply(na_cumsum, 1, diff))
  minmat <- apply(difmat, 1, which.min)
  
  df$do_idx <- ifelse(minmat == 1 & na_cumsum[,2] == 1, 1, # if index is 1 and only the last question was dropped, it's dropout of 1
                      ifelse(minmat == 1 & na_cumsum[,1] != 1, 0, # if index is 1 because nothing was dropped it should be 0
                             ifelse(na_cumsum[, length(q_pos)] == length(q_pos), length(q_pos), # if nothing was answered, the index should be length(q_pos),
                                    length(q_pos) - minmat)# otherwise it should give the last answered question
                      )
  )
  
  return(df)
}

