#' Add Dropout Index to a Data.Frame
#' 
#' @description
#' Find drop out positions in a data.frame that contains multiple 
#' questions/items that had been asked/worked on sequentially.
#' This function adds the Dropout Index variable `do_idx` to the data.frame which is necessary
#' for further analyses of dropout.
#' 
#' Use this function _first_ to prepare your dropout analysis. Then, keep going by creating
#' the dropout statistics using [compute_stats()].
#' 
#' @details
#' Importantly, this function will start counting missing data at the end of the 
#' data frame. Any missing data which is somewhere in between, i.e.
#' a single item that was skipped or forgotten will not be counted as dropout.
#' The function will identify sequences of missing data that go until the end of the 
#' data frame and add the number of the last answered question in `do_idx`.
#' 
#' The Dropout Index variable `do_idx` will therefore code the last item in sequence that is not NA.
#' If every item is not NA (every item was answered), the `do_idx` will be one longer than the number of items in the analysis
#' to ensure that this is not counted as dropout in further analyses, e.g. in [compute_stats()].
#' 
#' Therefore, the variables must be in the order that they were asked, otherwise analyses
#' will not be valid.
#' 
#' @param df data.frame containing `NA`s
#' @param q_pos numeric range of columns that contain question items
#'
#' @export
#' 
#' @returns Returns original data frame with column `do_idx` added.
#' @seealso [compute_stats()] which is usually the next step for dropout analysis.
#' 
#' @source R/add_dropout_idx.R
#' 
#' @examples
#' dropout <- add_dropout_idx(dropRdemo, 3:54)
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
  
  # New Version taking into account entirely empty rows
  # df$do_idx <- ifelse(minmat == 1 & na_cumsum[,2] == 1, length(q_pos)-1, # if index is 1 and only the last question was dropped, it's dropout of length(q_pos)-1
  #                     ifelse(minmat == 1 & na_cumsum[,1] != 1, length(q_pos)+1, # if index is 1 because nothing was dropped it should be length(q_pos) + 1 as a value that cannot exist with the data
  #                            ifelse(na_cumsum[, length(q_pos)] == length(q_pos), 0, # if nothing was answered, the index should be 0,
  #                                   length(q_pos) - minmat)# otherwise it should give the last answered question
  #                     )
  # )
  
  # New 1.0.4 version, more nicely readable :)
  idx <- length(q_pos) - minmat
  
  idx[minmat == 1 & na_cumsum[,2] == 1] <- length(q_pos) - 1
  idx[minmat == 1 & na_cumsum[,1] != 1] <- length(q_pos) + 1
  idx[na_cumsum[, length(q_pos)] == length(q_pos)] <- 0
  
  df$do_idx <- idx
  
  return(df)
}

