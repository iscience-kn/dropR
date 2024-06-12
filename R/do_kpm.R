#' Kaplan-Meier Survival Estimation
#' 
#' This function needs a data set with a dropout index added by [add_dropout_idx()].
#' The `do_kpm` function performs survival analysis with Kaplan-Meier Estimation 
#' and returns a list containing survival steps, the original data frame, and the model fit type. 
#' The function can fit the survival model either for the entire data set or separately by a specified condition column.
#' 
#' 
#' @param df data set with `do_idx` added by [add_dropout_idx()]
#' @param condition_col character denoting the experimental conditions to model
#' @param model_fit character Should be either "total" for a total model or "conditions"
#' @importFrom survival Surv survfit
#' @export
#' 
#' @returns a list containing `steps` (survival steps extracted from the fitted models), 
#' `d` (the original data frame), and `model_fit` (the model fit type).
#' 
#' @seealso [survival::Surv()] used to fit survival object.
#' 
#' @examples
#' demo_kpm <- do_kpm(df = add_dropout_idx(dropRdemo, 3:54),
#' condition_col = "experimental_condition",
#' model_fit = "total")
#' 
#' head(demo_kpm$steps)
#' 
do_kpm <- function(df,
                   condition_col = "experimental_condition",
                   model_fit = "total"){
  
  df$surv <- with(df, Surv(do_idx, do_idx != max(df$do_idx)))
  if(model_fit == "total"){
    fit1 <- survfit(surv~1, data = df)
    steps <- get_steps_by_cond(fit1, "total")
    steps
  } else {
    by_cond <- split(df, factor(df[ ,condition_col]))
    by_cond_fit <- lapply(by_cond,
                          function(x) survfit(surv~1, data = x))
    
    by_cond_fit <- lapply(names(by_cond_fit), function(x) {
      # Copy the current list element
      fit <- by_cond_fit[[x]]
      
      # Modify 'upper' and 'lower' variables as they throw errors for NAs (which can be substituted with 0)
      fit$upper <- ifelse(is.na(fit$upper), 0, fit$upper)
      fit$lower <- ifelse(is.na(fit$lower), 0, fit$lower)
      
      # Return the modified list element
      return(fit)
    })
    names(by_cond_fit) <- names(by_cond)
    
    by_cond_steps <- lapply(names(by_cond_fit), function(x){
      get_steps_by_cond(by_cond_fit[[x]], x)
    })
    
    steps <- do.call("rbind", by_cond_steps)
  }
  out <- list()
  out$steps <- steps
  out$d <- df
  out$model_fit <- model_fit
  out
}


#' Draw a Kaplan Meier Plot
#' 
#' The `do_kpm_plot` function generates a Kaplan-Meier survival plot based on the 
#' output from the [do_kpm()] function. It allows for customization of conditions 
#' to display, confidence intervals, color palettes, and y-axis scaling.
#' 
#' @param kds list object as modeled by [do_kpm()]
#' @param sel_conds character Which experimental conditions to plot. 
#' @param kpm_ci boolean Should there be confidence bands in the plot? Defaults to TRUE.
#' @param color_palette_kp character indicating which color palette to use. Defaults to 'color_blind',
#' alternatively choose 'gray' for gray scale values or 'default' for the ggplot2 default colors. 
#' @param full_scale_kpm boolean Should the Y axis show the full range from 0 to 100? Defaults to FALSE.
#' @import ggplot2
#' @export
#' 
#' @returns The function returns a `ggplot` object containing the Kaplan-Meier survival plot. Using the Shiny App version of
#' dropR, this plot can easily be downloaded in different formats. 
#' 
#' @examples
#' do_kpm_plot(do_kpm(d = add_dropout_idx(dropRdemo, 3:54),
#' condition_col = "experimental_condition",
#' model_fit = "total"))
#' 
#' do_kpm_plot(do_kpm(d = add_dropout_idx(dropRdemo, 3:54),
#' condition_col = "experimental_condition",
#' model_fit = "conditions"), sel_conds = c("11", "12", "21", "22"))
#' 
do_kpm_plot <- function(
    kds,
    sel_conds = c("11", "12", "21", "22"),
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
    k <- ggplot(subset(kds$steps, condition %in% sel_conds),
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
                         alpha=.25)
  }
  
  if(color_palette_kp != "default"){
    k <- k + scale_fill_manual(values=palette) +
      scale_color_manual(values=palette)
  }
  
  if(full_scale_kpm){
    k <- k + 
      scale_y_continuous(limits = c(0, 100))
  }
  
  k <- k + guides(color = guide_legend(title = NULL),
                  fill = guide_legend(title = NULL)) +
    xlab("Question Index") + 
    ylab("Survival in %")
  
  k
  
}

#' Test Survival Curve Differences
#' 
#' This function compares survival curves as modeled with [do_kpm()].
#' It outputs a contingency table and a Chisq measure of difference.
#' 
#' @importFrom survival survdiff
#' @param df data set of a survival model such as [do_kpm()]
#' @param cond character of experimental condition variable in the data
#' @param test_type numeric (0 or 1) parameter that controls the type of test (0 means rho = 0; log-rank,
#' 1 means rho = 1; Peto & Peto Wilcox)
#' @export
#' 
#' @examples
#' kpm_est <- do_kpm(add_dropout_idx(dropRdemo, 3:54))
#' get_survdiff(kpm_est$d, "experimental_condition", 0)
#' get_survdiff(kpm_est$d, "experimental_condition", 1)
#' 
#' 
get_survdiff <- function(df, cond, test_type){
  f <- as.formula(paste("surv", cond, sep="~"))
  survdiff(f, data = df, rho = test_type)  
}
