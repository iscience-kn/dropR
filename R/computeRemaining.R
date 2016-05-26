#' Compute Share of Remaining Participants
#' 
#' Given a one-row-one-observation, one-column-one-question format data.frame,
#' which uses R compliant coding (i.e. NA) for missing values,
#' this function computes the share of remaining participants for each question.
#' 
#' @param dframe a data.frame
#' @param number of questions
#' @export
computeRemaining <- function(dframe,n_cols,
                             by_cond = NULL,
                             long_format = T,
                             d_col = "drop_out",
                             strip_last_question = T){
  if(is.null(by_cond)){
    N <- nrow(dframe)
    s <- sapply(1:n_cols,function(x) n_count(dframe[,d_col],x))
    cs <- cumsum(s)
    out_df <- data.frame(id = 1:n_cols,count = s,cs = cs,
                         remain_pct = (N-cs)/N)
    if(strip_last_question) out_df <- out_df[-nrow(out_df),]
    out_df
  } else {
    # some sanity check
    stopifnot(by_cond %in% names(dframe))
    sp <- split(dframe,factor(dframe[,by_cond]))
    
    out <- lapply(sp,function(x){
      N <- nrow(x)
      s <- sapply(1:n_cols,function(z) n_count(x[,d_col],z))
      cs <- cumsum(s)
      out_df <- data.frame(id = 1:n_cols,count = s,cs = cs,
                           pct_remain = (N-cs)/N)
      if(strip_last_question) out_df <- out_df[-nrow(out_df),]
      out_df$pct_remain
    })
    
    # create nice long format data.frame based output
    # using base R
    
    df_out <- as.data.frame(out)
    df_lng <- reshape(df_out,direction="long",
                      v.names="pct_remain",
                      varying = list(1:ncol(df_out))
    )
    names(df_lng)[1] <- "condition"
    df_lng$condition <- as.factor(df_lng$condition)
    levels(df_lng$condition) <- names(df_out)
    
    # long format is better for use with ggplot and ggvis    
    if(long_format){
      df_lng  
    } else{
      df_out
    }
  }
}
