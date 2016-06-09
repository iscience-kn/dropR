library(dropR)
library(data.table)
data("dropRdemo")
n_q <- length(grep("vi_",names(dropRdemo)))
dropRdemo$drop_out_idx <- extract_drop_out_from_df(dropRdemo,grep("vi_",names(dropRdemo)))

test <- computeStatistics(dropRdemo,by_cond = "experimental_condition",
                          no_of_vars = 52)

test <- computeStatistics(dropRdemo,by_cond = "None",
                          no_of_vars = 52)

d <- test
d <- as.data.frame(stats())
d$condition <- factor(d$condition)

d <- subset(d,condition != "total")
test_input <- subset(d,drop_out_idx == 10)
# chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
chisq.test(as.table(matrix(test_input[,c("condition","cs","remain")])))

chisq.test(as.table(as.matrix(test_input[,list(condition,cs,remain)])))

Q <- as.table(as.matrix(test_input[,list(condition,cs,remain)]))

chisq.test(Q)
## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary

test
test$cond
head(test$total)

debug(computeStatistics)



library(data.table)
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

undebug(computeStatistics)
test <- computeStatistics(dropRdemo,by_cond = "experimental_condition",
                          no_of_vars = 52)

tt <- subset(test,drop_out_idx == 45)
ttt <- as.table(as.matrix(tt[,list(experimental_condition,cs,remain)]))
chisq.test(ttt)


dt <- data.table(dropRdemo)
out <- dt[,list(drop_out_count = .N),
       keyby = c("drop_out_idx","experimental_condition")]

out <- merge(out,
             data.table(id = sort(rep(1:52,4)),
                        ec = c(11,12,21,22)),
             by.x = c("drop_out_idx","experimental_condition"),
             by.y = c("id","ec"),
             all.y = T,
             allow.cartesian = T
)

out[is.na(drop_out_count),drop_out_count := 0,]

out[,cs := cumsum(drop_out_count),
    keyby = c("experimental_condition")]

test <- test[dt[,.N,keyby = "experimental_condition"]]
out[, remain := N-cs]
out[, pct_remain := (N-cs)/N]


a <- dt[,list(total = .N),keyby = "drop_out_idx"]
a[dt[,list(N_f = .N)]]
b <- out[,keyby = "drop_out_idx"]
a
merge(a,b,by="drop_out_idx")


dtable[,list(drop_out_count = .N),
       keyby = list(do_indicator,by_cond)]


aa <- computeStatistics(dropRdemo,by_cond = "experimental_condition")
aa <- merge(aa,data.table(id = 1:52),
            by.x = "drop_out_idx",
            by.y = "id",
            all.y = T)
aa[is.na(drop_out_count),drop_out_count := 0,]
aa[,cs := cumsum(drop_out_count)]

aa[,id := CJ(1:52),]
SJ

xx <- computeRemaining(dropRdemo,n_q)

library(data.table)
dt1 <- data.table(dropRdemo)

setkeyv(dt1,"experimental_condition")




xx <- dt1[,.N,by=list(experimental_condition,drop_out)]
order(xx,)



n_count(dt1$drop_out,10)

table(dt1$drop_out)
subset(xx,drop_out == 52)
