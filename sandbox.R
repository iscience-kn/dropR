library(ggvis)
source("R/extract_drop_out_from_df.R")
source("R/find_drop_out.R")
source("R/utils.R")
source("R/computeRemaining.R")

# test dataset
data_in <- read.csv2("data/proper_input.csv")

# outputs position of last record that was not NA... 
data_in$drop_out <- extract_drop_out_from_df(data_in,paste0("q",1:10))
#extract_drop_out_from_df(data_in,paste0("q",1:10))

# set parameters (later on from shiny) ####
# number of questions / cols
n_q <- length(2:11)
# total share



total_dropout <- computeRemaining(data_in,n_q)
tdo_df <- data.frame(condition = "total", pct_remain = total_dropout$remain_pct,
           id = total_dropout$id)
dropout_by_grp <- computeRemaining(data_in,n_q,"groups")
dropout_by_grp_full <- rbind(tdo_df,dropout_by_grp)

# some plotting test
dropout_by_grp_full%>%
  ggvis() %>%
  group_by(condition) %>%
  layer_paths(~id,~pct_remain,stroke=~condition) %>%
  layer_points(~id,~pct_remain,fill=~condition,fill="transparent") %>%
  add_tooltip(all_values,"hover")


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
