library(dropR)
data("dropRdemo")
n_q <- length(grep("vi_",names(dropRdemo)))
dropRdemo$drop_out_idx <- extract_drop_out_from_df(dropRdemo,grep("vi_",names(dropRdemo)))

library(data.table)
computeStatistics <- function(df, by_cond = NULL,
                              do_indicator = "drop_out_idx"
                              ){
  dtable <- data.table(df)
#  if(is.null(by_cond)){
    out <- dtable[,list(drop_out_count = .N),
                  keyby = list(do_indicator,by_cond)]
  #  out[,cs := cumsum(count_remain)]
    out
 # }
  
}

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

out <- out[dt[,.N,keyby = "experimental_condition"]]
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
