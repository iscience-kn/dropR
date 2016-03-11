library(ggvis)
source("R/extract_drop_out_from_df.R")
source("R/find_drop_out.R")
source("R/utils.R")
source("R/computeRemaining.R")

# test dataset
data_in <- read.csv2("data/proper_input.csv")

# outputs position of last record that was not NA... 
data_in$drop_out <- extract_drop_out_from_df(data_in,2:11)
# set parameters (later on from shiny) ####
# number of questions / cols
n_q <- length(2:11)
# total share

drpout <- as.data.frame(table(data_in$drop_out))
drpout$cs <- cumsum(drpout$Freq)
drpout$remain <- sum(drpout$Freq)-drpout$cs
drpout$remain_pct <- drpout$remain/sum(drpout$Freq)


total_dropout <- computeRemaining(data_in,n_q)
dropout_by_grp <- computeRemaining(data_in,n_q,"groups")

# some plotting test
ggvis(dropout_by_grp) %>%
  group_by(condition) %>%
  layer_paths(~id,~pct_remain,stroke=~condition)

