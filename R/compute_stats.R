#' Compute Statistics
#' 
#' @param df data.frame
#' @param by_cond character group by condition, defaults to None.
#' @param do_indicator character dropout indicator
#' @param no_of_vars numeric number of variables
#'
#' @export
compute_stats <- function(df, by_cond = "None",
                              do_indicator = "drop_out_idx",
                              no_of_vars
){
  out <- list()
  dtable <- data.table(df)
  
  if(by_cond == "None"){
    out$cond <- NULL
  } else {
    # drop out count by conditions
    do_by_cond <- dtable[,list(drop_out_count = .N),
                         keyby = c(do_indicator,by_cond)]
    # expand to full grid 
    no_of_cond <- length(unique(dtable[,get(by_cond)]))
    full_grid <- merge(do_by_cond,
                       data.table(id = sort(rep(1:no_of_vars,4)),
                                  ec = unique(dtable[,get(by_cond)])),
                       by.x = c(do_indicator,by_cond),
                       by.y = c("id","ec"),
                       all.y = T,
                       allow.cartesian = T)
    
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
    out$cond <- full_grid[,c("drop_out_idx","condition","cs","N","remain",
                              "pct_remain"),with = FALSE]
  } 
  
  do_by_total <- dtable[,list(drop_out_count = .N),
                        keyby = c(do_indicator)]
  total_grid <- merge(do_by_total,
                      data.table(id = rep(1:no_of_vars)),
                      by.x = do_indicator,
                      by.y = "id",
                      all.y = T,
                      allow.cartesian = T)
  
  total_grid[is.na(drop_out_count), drop_out_count := 0,]
  total_grid[,cs := cumsum(drop_out_count)]
  total_grid[,N := dtable[,.N]]
  total_grid[, remain := N-cs]
  total_grid[, pct_remain := (N-cs)/N]  
  total_grid[, condition := "total"]
  total_grid$condition <- factor(total_grid$condition)
  out$total <- total_grid[,c("drop_out_idx","condition","cs","N","remain",
                             "pct_remain"),with = FALSE]
  
  rbind(out$total, out$cond)
  
}
