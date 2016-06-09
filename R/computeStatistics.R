#' Compute Statistics
#' 
#' 
#' @export
computeStatistics <- function(df, by_cond = NULL,
                              do_indicator = "drop_out_idx",
                              no_of_vars
){
  out <- list()
  dtable <- data.table(df)
  
  if(is.null(by_cond)){
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
    out$cond <- full_grid[]
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
  out$total <- total_grid[]
  
  out
  
}
