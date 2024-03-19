#' Kaplan-Meier 
#' 
#' 
#' 
#' @importFrom survival Surv survfit
#' @export
do_kpm <- function(d,
                   qs,
                   condition_col,
                   model_fit){
  d <- add_dropout_idx(d, qs)
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


#' @import ggplot2
#' @export
do_kpm_plot <- function(
    kds,
    sel_cond_kpm,
    kpm_ci,
    color_palette_kp,
    full_scale_kpm
){
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
          axis.line = element_line(colour = "black"))
  if(kpm_ci){
    k <- k + geom_ribbon(aes(ymin = lwr*100,
                             ymax = uppr*100),
                         alpha=.3)
  }
  
  if(color_palette_kp == "color_blind"){
    k <- k + scale_fill_manual(values=c("#000000", "#E69F00",
                                        "#56B4E9", "#009E73",
                                        "#F0E442", "#0072B2",
                                        "#D55E00", "#CC79A7")) +
      scale_color_manual(values=c("#000000", "#E69F00",
                                  "#56B4E9", "#009E73",
                                  "#F0E442", "#0072B2",
                                  "#D55E00", "#CC79A7"))
  }
  
  if(color_palette_kp == "gray"){
    k <- k +
      scale_color_manual(values = gray(seq(from=0,1,
                                           by=1/8)[c(1,8,3,7,4,5,2,6)]
      )) +
      scale_fill_manual(values = gray(seq(from=0,1,
                                          by=1/8)[c(1,8,3,7,4,5,2,6)])
      )
  }
  
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


#' @importFrom survival survdiff
#' @export
get_survdiff <- function(d, cond, test_type){
  f <- as.formula(paste("surv", cond, sep="~"))
  survdiff(f, data = d, rho = test_type)  
}
