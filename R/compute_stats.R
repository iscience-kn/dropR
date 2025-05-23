#' Compute Dropout Statistics
#' 
#' This is the _second step_ in conducting dropout analysis with `dropR`. 
#' Outputs all necessary statistics to analyze and visualize dropout, such as 
#' the sample size N of the data (and in each condition if selected), cumulative 
#' dropout and remaining participants in absolute numbers and percent.
#' If no experimental condition is added, the stats are only calculated for the 
#' whole data in total.
#' 
#' @param df data.frame containing variable `do_idx` from [add_dropout_idx()]
#' @param by_cond character name of condition variable in the data, defaults to 'None' to output total statistics.
#' @param no_of_vars numeric number of variables that contain questions
#' @param excl_cond_NA boolean Exclude NAs in the condition variable? This typically means that there is an entirely empty row in your data,
#' i.e. someone quit before being assigned an experimental condition. This is not technically dropout and will skew results. Default is `TRUE`.
#'
#' @import data.table
#' @export
#' 
#' @returns A data frame with 6 columns (q_idx, condition, cs, N, remain, pct_remain)
#' and as many rows as questions in original data (for overall data and if conditions selected
#' again for each condition).
#' 
#' @examples
#' do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#'   
compute_stats <- function(df,
                          by_cond = "None",
                          no_of_vars,
                          excl_cond_NA = T){
  # Resolve global variable issue
  drop_out_count <- cs <- remain <- N <- pct_remain <- condition <- NULL
  
  # dtable <- data.table(df)
  
  if(by_cond %in% names(df)){ # if experimental condition is actually in the data
    # drop out count by conditions
    
    if(excl_cond_NA){ # if condition is NA 
      df <- df[!is.na(df[by_cond]),]
    }
    
    dtable <- data.table(df)
    
    do_by_cond <- dtable[,list(drop_out_count = .N),
                         keyby = c("q_idx" = "do_idx", by_cond)]
    # expand to full grid 
    no_of_cond <- length(unique(dtable[, get(by_cond)]))
    full_grid <- merge(do_by_cond,
                       data.table(id = sort(rep(0:no_of_vars, no_of_cond)), # add 0 as possible q_idx for dropout
                                  ec = unique(dtable[, get(by_cond)])),
                       by.x = c("q_idx", by_cond),
                       by.y = c("id","ec"),
                       all.y = TRUE,
                       allow.cartesian = TRUE)
    
    full_grid[is.na(drop_out_count), drop_out_count := 0,]
    
    # add cumulative dropout count by condition
    full_grid[,cs := cumsum(drop_out_count),
              keyby = by_cond]
    # add observations per condition to compute remain
    full_grid <- full_grid[dtable[,.N,keyby = by_cond]]
    # add remaining and pct remaining
    full_grid[, remain := N-cs]
    full_grid[, pct_remain := (N-cs)/N]
    # rename the condition field to a fixed name for easy rbind later on
    name_pos <- match(by_cond,names(full_grid))
    names(full_grid)[name_pos] <- "condition"
    full_grid$condition <- factor(full_grid$condition)
    
    cond_grid <- full_grid[,c("q_idx","condition","cs","N","remain",
                              "pct_remain"),with = FALSE]
  } else{
    dtable <- data.table(df)
    cond_grid <- NULL
  }
  
  do_by_total <- dtable[,list(drop_out_count = .N),
                        keyby = c("q_idx" ="do_idx")]
  
  total_grid <- merge(do_by_total,
                      data.table(id = rep(0:no_of_vars)),
                      by.x = "q_idx",
                      by.y = "id",
                      all.y = TRUE,
                      allow.cartesian = TRUE)
  
  total_grid[is.na(drop_out_count), drop_out_count := 0,]
  total_grid[,cs := cumsum(drop_out_count)]
  total_grid[,N := dtable[,.N]]
  total_grid[, remain := N-cs]
  total_grid[, pct_remain := (N-cs)/N]  
  total_grid[, condition := "total"]
  total_grid$condition <- factor(total_grid$condition)
  
  all_grid <- rbind(total_grid[,c("q_idx","condition","cs","N","remain",
                                  "pct_remain"),with = FALSE],
                    cond_grid)
  
  all_grid <- all_grid[q_idx < no_of_vars, ] # remove last row per condition, which currently equals the second to last row
  all_grid[,"q_idx"] <- all_grid[,"q_idx"]+1 # add one so calculation now correctly depicts the remaining percent at each item
  
  return(all_grid)
}
