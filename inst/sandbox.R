library(dropR)
library(data.table)
data("dropRdemo")
n_q <- length(grep("vi_",names(dropRdemo)))
dropRdemo <- add_dropout_idx(dropRdemo, grep("vi_",names(dropRdemo)))

test <- compute_stats(dropRdemo, by_cond = "experimental_condition",
                          no_of_vars = 52)

test <- compute_stats(dropRdemo, by_cond = "None",
                          no_of_vars = 52)



share_remain <-  data.frame(intermediate()$remain)$Freq / data.frame(intermediate()$participants)$Freq

odds <- get_odds(share_remain)
condition <- data.frame(intermediate()$participants)$Var1

m <- combn(odds,2)
or <- m[1,]/m[2,]
nms <- combn(as.character(condition),
             2,paste0,collapse=' vs. ')

d <- data.frame(t(or))
colnames(d) <- nms
d


get_odds <- function(p){
  if(!all(p <= 1 & p >= 0)) stop('Input is not a probability!')
  p / (1-p)
} 

test <- subset(test,condition != "total")
test_input <- subset(test,drop_out_idx == 10)

get_odds_ratio <- function(a,b){
 get_odds(a)/get_odds(b)
}

OR_matrix <- outer(test_input$pct_remain,test_input$pct_remain,FUN = get_odds_ratio)
colnames(OR_matrix) <- test_input$condition
row.names(OR_matrix) <- test_input$condition



debug(get_or_matrix)
oo <- get_or_matrix(test_input,"condition")

mm <- matrix(as.character(oo),2)
apply(mm,2,function(x) paste0(x[1], x[2]))





t(combn(test_input$condition,2))

get_odds(test_input$pct_remain[1])/
get_odds(test_input$pct_remain[2])



d <- test
d <- as.data.frame(stats())
d$condition <- factor(d$condition)

d <- subset(d,condition != "total")

# chisq.test(as.table(as.matrix(test_input[,c("condition","cs","remain")])))
tbl <- test_input[,c("cs","remain"),with=FALSE]
tbl2 <- as.table(as.matrix(tbl))
dimnames(tbl2) <- list(conditions = as.character(test_input$condition),
                       participants = c("dropout","remain"))


chisq.test(tbl2)
renderTable(tbl2)


tbl <- as.table(matrix(test_input[,list(condition,cs,remain)]))

chisq.test()

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

debug(compute_stats)



library(data.table)
compute_stats <- function(df, by_cond = NULL,
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

undebug(compute_stats)
test <- compute_stats(dropRdemo,by_cond = "experimental_condition",
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


aa <- compute_stats(dropRdemo,by_cond = "experimental_condition")
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




xx <- dt1[,.N,by=list(experimental_condition,do_idx)]
order(xx,)



n_count(dt1$do_idx,10)

table(dt1$do_idx)
subset(xx,do_idx == 52)
