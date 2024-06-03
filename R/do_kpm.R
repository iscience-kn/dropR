#' Kaplan-Meier Survival Estimation
#' 
#' This function needs a data set with a dropout index added by [add_dropout_idx()].
#' 
#' 
#' 
#' @param d dataset with do_idx added by [add_dropout_idx()]
#' @param condition_col character denoting the experimental conditions to model
#' @param model_fit character Should be either "total" for a total model or denote only select
#' experimental conditions
#' @importFrom survival Surv survfit
#' @export
#' 
#' @examples
#' demo_kpm <- do_kpm(d = add_dropout_idx(dropRdemo, 3:54),
#' condition_col = "experimental_condition",
#' model_fit = "total")
#' 
do_kpm <- function(d,
                   # q_pos,
                   condition_col = "experimental_condition",
                   model_fit = "total"){
  
  # d <- add_dropout_idx(d, q_pos)
  
  # q_pos character or numeric position vector of questions column indicator.
  
  
  d$surv <- with(d, Surv(do_idx, do_idx != max(d$do_idx)))
  if(model_fit == "total"){
    fit1 <- survfit(surv~1, data = d)
    steps <- get_steps_by_cond(fit1, "total")
    steps
  } else {
    by_cond <- split(d, factor(d[ ,condition_col]))
    by_cond_fit <- lapply(by_cond,
                          function(x) survfit(surv~1, data = x))
    
    by_cond_steps <- lapply(names(by_cond_fit), function(x){
      get_steps_by_cond(by_cond_fit[[x]], x)
    })
    
    steps <- do.call("rbind", by_cond_steps)
  }
  out <- list()
  out$steps <- steps
  out$d <- d
  out$model_fit <- model_fit
  out
}


#' Draw a Kaplan Meier Plot
#' 
#' @param kds object as modelled by [do_kpm()]
#' @param sel_cond_kpm select experimental conditions. 
#' @param kpm_ci boolean Should there be confidence bands in the plot?
#' @param color_palette_kp character indicating which color palette to use. Defaults to 'color_blind',
#' alternatively choose 'gray' or 'default' for the ggplot2 default colors. 
#' @param full_scale_kpm boolean Should the Y axis show the full range from 0 to 100?
#' @import ggplot2
#' @export
#' 
#' @examples
#' do_kpm_plot(do_kpm(d = add_dropout_idx(dropRdemo, 3:54),
#' condition_col = "experimental_condition",
#' model_fit = "total"))
#' 
do_kpm_plot <- function(
    kds,
    sel_cond_kpm = "experimental_condition",
    kpm_ci = T,
    color_palette_kp = "color_blind",
    full_scale_kpm = F
){
  palette <- if(color_palette_kp == "color_blind"){c("#000000", "#E69F00",
                                                  "#56B4E9", "#009E73",
                                                  "#F0E442", "#0072B2",
                                                  "#D55E00", "#CC79A7")
  } else {gray(seq(from = 0,1,
                   by = 1/8)[c(1,8,3,7,4,5,2,6)])}
  
  if(kds$model_fit == "conditions"){
    k <- ggplot(subset(kds$steps, condition %in% sel_cond_kpm),
                aes(x,y*100, col = condition, fill = condition))
  } else {
    k <- ggplot(subset(kds$steps, condition %in% "total"),
                aes(x, y*100, col = condition, fill = condition))
  }
  
  k <- k + 
    geom_line() +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16))
  if(kpm_ci){
    k <- k + geom_ribbon(aes(ymin = lwr*100,
                             ymax = uppr*100),
                         alpha=.3)
  }
  
  if(color_palette_kp != "default"){
    k <- k + scale_fill_manual(values=palette) +
      scale_color_manual(values=palette)
  }
  
  # if(color_palette_kp == "color_blind"){
  #   k <- k + scale_fill_manual(values=c("#000000", "#E69F00",
  #                                       "#56B4E9", "#009E73",
  #                                       "#F0E442", "#0072B2",
  #                                       "#D55E00", "#CC79A7")) +
  #     scale_color_manual(values=c("#000000", "#E69F00",
  #                                 "#56B4E9", "#009E73",
  #                                 "#F0E442", "#0072B2",
  #                                 "#D55E00", "#CC79A7"))
  # }
  # 
  # if(color_palette_kp == "gray"){
  #   k <- k +
  #     scale_color_manual(values = gray(seq(from=0,1,
  #                                          by=1/8)[c(1,8,3,7,4,5,2,6)]
  #     )) +
  #     scale_fill_manual(values = gray(seq(from=0,1,
  #                                         by=1/8)[c(1,8,3,7,4,5,2,6)])
  #     )
  # }
  
  if(full_scale_kpm){
    k <- k + 
      scale_y_continuous(limits = c(0, 100))
  }
  
  k <- k + guides(color = guide_legend(title = NULL),
                  fill = guide_legend(title = NULL)) +
    xlab("Question Index") + 
    ylab("Percent Remaining")
  
  k
  
}

#' Test Survival Curve Difference
#' 
#' @importFrom survival survdiff
#' @param d description
#' @param cond description
#' @param test_type a scalar parameter that controls the type of test
#' @export
get_survdiff <- function(d, cond, test_type){
  f <- as.formula(paste("surv", cond, sep="~"))
  survdiff(f, data = d, rho = test_type)  
}
