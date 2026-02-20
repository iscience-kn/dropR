# Running Dropout related Tests with dropR

``` r
library(dropR)
data("dropRdemo")
qs <- which(grepl("vi_", names(dropRdemo)))
dropRdemo <- add_dropout_idx(dropRdemo, q_pos = qs)

stats <- compute_stats(dropRdemo,
                             by_cond = "experimental_condition",
                             no_of_vars = length(qs))
```

### Running a Chi-sq. Test for a Selected Question

> ***NOTE:*** Chi-sq. tests are only available when an column denoting
> the experimental condition is given.

Let’s compute a Chi-Squared test at question 15 for experimental
conditions `11` and `12`. dropR’s dropout Chi-Squared test returns a
list containing the actual *test results* and dropout overview table at
the selected question.

``` r
do_chisq(stats,
         chisq_question = 15,
         sel_cond_chisq = c('11','12'),
         p_sim = TRUE)
#> $`Test result`
#> 
#>  Pearson's Chi-squared test with simulated p-value (based on 2000
#>  replicates)
#> 
#> data:  test_table
#> X-squared = 1.9315, df = NA, p-value = 0.2214
#> 
#> 
#> $`Dropout up to item 15`
#>           participants
#> conditions dropout remaining
#>         11      14        58
#>         12       6        51
#> 
#> attr(,"class")
#> [1] "do_chi" "list"
```

### Kaplan-Meier Estimation

``` r
kpm <- do_kpm(df = add_dropout_idx(dropRdemo, qs),
              condition_col = "experimental_condition",
              model_fit = "total")

kpm$model_fit
#> [1] "total"
head(kpm$steps)
#>   x         y      uppr       lwr condition
#> 1 1 0.9593496 0.9843472 0.9349868     total
#> 2 2 0.9593496 0.9843472 0.9349868     total
#> 3 2 0.9471545 0.9755285 0.9196058     total
#> 4 3 0.9471545 0.9755285 0.9196058     total
#> 5 3 0.9105691 0.9469365 0.8755984     total
#> 6 4 0.9105691 0.9469365 0.8755984     total
head(kpm$d)
#>   obs_id experimental_condition vi_1 vi_2 vi_3 vi_4 vi_5 vi_6 vi_7 vi_8 vi_9
#> 1 7a9f33                     11    1    1    1    1    1    1    1    1    1
#> 2 e11f94                     22    1   NA    1    1    1    1   NA   NA   NA
#> 3 e72a50                     22    1   NA    1    1    1    1    1    1    1
#> 4 f90f5f                     11    1    1    1    1    1    1    1    1    1
#> 5 20bc72                     12    1   NA    1    1    1    1    1    1    1
#> 6 76b97a                     22    1   NA    1    1    1    1    1    1    1
#>   vi_10 vi_11 vi_12 vi_13 vi_14 vi_15 vi_16 vi_17 vi_18 vi_19 vi_20 vi_21 vi_22
#> 1     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 2    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> 3     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 4     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 5     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 6     1     1     1     1     1     1     1     1     1     1     1     1     1
#>   vi_23 vi_24 vi_25 vi_26 vi_27 vi_28 vi_29 vi_30 vi_31 vi_32 vi_33 vi_34 vi_35
#> 1     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 2    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> 3     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 4     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 5     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 6     1     1     1     1     1    NA    NA    NA    NA    NA    NA    NA    NA
#>   vi_36 vi_37 vi_38 vi_39 vi_40 vi_41 vi_42 vi_43 vi_44 vi_45 vi_46 vi_47 vi_48
#> 1     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 2    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> 3     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 4     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 5     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 6    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>   vi_49 vi_50 vi_51 vi_52 do_idx surv
#> 1     1     1     1     1     53  53+
#> 2    NA    NA    NA    NA      6    6
#> 3     1     1     1     1     53  53+
#> 4     1     1     1     1     53  53+
#> 5     1     1     1     1     53  53+
#> 6    NA    NA    NA    NA     27   27
```
