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
                          no_of_vars){
  # Resolve global variable issue
  drop_out_count <- cs <- remain <- N <- pct_remain <- condition <- NULL
  
  dtable <- data.table(df)
  
  if(by_cond %in% names(df)){ # if experimental condition is actually in the data
    # drop out count by conditions
    do_by_cond <- dtable[,list(drop_out_count = .N),
                         keyby = c("q_idx" = "do_idx", by_cond)]
    # expand to full grid 
    no_of_cond <- length(unique(dtable[, get(by_cond)]))
    pre_grid <- merge(do_by_cond,
                       data.table(id = sort(rep(1:no_of_vars, no_of_cond)),
                                  ec = unique(dtable[, get(by_cond)])),
                       by.x = c("q_idx", by_cond),
                       by.y = c("id","ec"),
                       all.y = TRUE,
                       allow.cartesian = TRUE)
    ####
    # NEW APPROACH
    # ADD SOME DUMMY DOCING THAT AT QUESTION 0 NO CONDITION HAS ANY DROPOUT
    
    # The issue is with fully empty rows and also if the dropout occurs after question one the coding gets whacky because item 1 is the starting point.
    # So, solution: add a dummy line with q_idx = 0 for each condition in the data and dropout_count = NA.
    # Then, if in the full_grid until then there is any dropout in the last available question (aka Q_idx == no_of_vars) then 
    # recode that to dropout_count = NA and add the dropout to position q_idx == 0
    
    dummy_grid <- data.frame(q_idx = rep(0, no_of_cond),
                             condition =  unique(df[by_cond]), 
                             drop_out_count = rep(NA, no_of_cond))
    full_grid <- rbind(pre_grid, dummy_grid)[order(condition, q_idx)]
    
    # move the value of dropout equal to number of questions (a.k.a. empty data altogether) to q_idx==0
    full_grid2 <- full_grid
    # For each unique condition
    for (cond in unique(full_grid$condition)) {
      val <- with(full_grid, drop_out_count[condition == cond & q_idx == 10])
      
      if (length(val) == 1 && !is.na(val)) {
        full_grid$drop_out_count[full_grid$condition == cond & full_grid$q_idx == 0] <- val # add number of drops
        full_grid$drop_out_count[full_grid$condition == cond & full_grid$q_idx == 10] <- NA # remove drop from last question
      }
    }
    
    
    ####
    
    full_grid[is.na(drop_out_count), drop_out_count := 0,]
    
    # add cumulative dropout count by condition
    full_grid[,cs := cumsum(drop_out_count),
              keyby = by_cond]
    # add observations per condition to compute remain
    full_grid <- full_grid[dtable[,.N,keyby = by_cond]]
    # add remaining and pct remaining
    full_grid[, remain := N-cs]
    full_grid[, pct_remain := (N-cs)/N]
    # rename the condition field to a fixed name for easy
    # rbind later on
    name_pos <- match(by_cond,names(full_grid))
    names(full_grid)[name_pos] <- "condition"
    full_grid$condition <- factor(full_grid$condition)
    
    cond_grid <- full_grid[,c("q_idx","condition","cs","N","remain",
                              "pct_remain"),with = FALSE]
  } else{
    cond_grid <- NULL
  }
  ## Alternative coding for consistency with new approach
  # do_by_total <- dtable[,list(drop_out_count = .N),
  #                       keyby = c("q_idx" ="do_idx")]
  bla <- tidyr::pivot_wider(full_grid, names_from = q_idx, values_from = drop_out_count)
  drops <- colSums(bla[, 6:(no_of_vars+6)], na.rm = T) 
  total_grid <- data.table(q_idx = as.numeric(names(drops)),
                            drop_out_count = drops)
  
  # total_grid <- merge(do_by_total,
  #                     data.table(id = rep(1:no_of_vars)),
  #                     by.x = "q_idx",
  #                     by.y = "id",
  #                     all.y = TRUE,
  #                     allow.cartesian = TRUE)
  ##
  
  total_grid[is.na(drop_out_count), drop_out_count := 0,]
  total_grid[,cs := cumsum(drop_out_count)]
  total_grid[,N := dtable[,.N]]
  total_grid[, remain := N-cs]
  total_grid[, pct_remain := (N-cs)/N]  
  total_grid[, condition := "total"]
  total_grid$condition <- factor(total_grid$condition)

  rbind(total_grid[,c("q_idx","condition","cs","N","remain",
                      "pct_remain"),with = FALSE],
        cond_grid)
}
