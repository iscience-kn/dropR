library(ggvis)
source("R/extract_drop_out_from_df.R")
source("R/find_drop_out.R")
source("R/utils.R")
source("R/computeRemaining.R")

# test dataset
data_in <- read.csv2("data/proper_input.csv")
data_in <- read.csv2("data/otherfile.csv")


# outputs position of last record that was not NA... 
data_in$drop_out <- extract_drop_out_from_df(data_in,paste0("q",1:10))
data_in$drop_out <- extract_drop_out_from_df(data_in,paste0("vi_",1:52))
#extract_drop_out_from_df(data_in,paste0("q",1:10))

# set parameters (later on from shiny) ####
# number of questions / cols
n_q <- length(1:52)
# total share



total_dropout <- computeRemaining(data_in,n_q)
tdo_df <- data.frame(condition = "total", pct_remain = total_dropout$remain_pct,
           id = total_dropout$id)
dropout_by_grp <- computeRemaining(data_in,n_q,".wx.3.experimental_condition")
dropout_by_grp_full <- rbind(tdo_df,dropout_by_grp)

# some plotting test
dropout_by_grp_full%>%
  ggvis() %>%
  group_by(condition) %>%
  layer_paths(~id,~pct_remain,stroke=~condition) %>%
  layer_points(~id,~pct_remain,fill=~condition) #%>%
#  add_tooltip(all_values,"hover")
dropout_by_grp_full %>%
  ggvis() %>%
  group_by(condition) %>%
  layer_paths(~id,~pct_remain,stroke=~condition) %>%
  layer_model_predictions(model = "lm")


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

# Kaplan Meier estimator
library(survival)

data_in$surv <- with(data_in,Surv(drop_out,drop_out != 52 ))
fit1 <- survfit(surv ~ 1, data = data_in,
                conf.type = "log-log")

plot(fit1$surv,type="l",ylim=c(0,1),col="blue")
lines(fit1$upper,col="red",lty="dashed")
lines(fit1$lower,col="red",lty="dashed")


test_df <- data.frame(questions = 1:length(fit1$surv),
                      upper = fit1$upper,
                      lower = fit1$lower,
                      fit = fit1$surv)

ggvis(test_df) %>%
  # layer_paths(~questions,~fit) %>%
  # layer_paths(~questions,~lower) %>%
  layer_paths(~questions,~upper)


age <- 1:10
y.low <- rnorm(length(age), 150, 25) + 10*age
y.high <- rnorm(length(age), 250, 25) + 10*age

plot(age,y.high,type = 'n', ylim = c(100, 400),
     ylab = 'Y Range', xlab = 'Age (years)')
lines(age, y.low, col = 'grey')
lines(age, y.high, col = 'grey')

lines(c(age, rev(age)),c(y.high, rev(y.low)))

polygon(c(age, rev(age)), c(y.high, rev(y.low)),
        col = "grey30", border = NA)


test_df <- data.frame(x = c(1:length(fit1$upper),rev(1:length(fit1$upper))),
                      y = c(fit1$upper,rev(fit1$lower))
                      )

test_df2 <- data.frame(x = 1:length(fit1$surv),y = fit1$surv)

df <- data.frame(x = c(1, 1, 2, 2), y = c(2, 1, 1, 2))
df %>% ggvis(~x, ~y, stroke := "red") %>% layer_paths()


library("maps")
texas <- ggplot2::map_data("state", region = "texas")



test_df %>% ggvis() %>%
  layer_paths(data = test_df2,~x,~y,strokeWidth := 3) %>%
  layer_paths(~x,~y,fill="blue", opacity := 0.4) 
  

  





