# Compute Kolmogorov-Smirnov Test for most extreme conditions

This test is used for survival analysis between the most extreme
conditions, so the ones with the most different rates of dropout. This
function automatically prepares your data and runs
[`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html) on it.

## Usage

``` r
do_ks(do_stats, question)
```

## Arguments

- do_stats:

  A data frame made from
  [`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md),
  containing information on the percent remaining per question per
  condition

- question:

  Index of question to be included in analysis, commonly the last
  question of the survey.

## Value

Returns result of Kolmogorov-Smirnoff test including which conditions
have the most different dropout rates.

## Examples

``` r
do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)

do_ks(do_stats, 52)
#> 
#>  Exact two-sample Kolmogorov-Smirnov test of conditions 12 & 22 at
#>  question 52
#> 
#> data:  do_stats$pct_remain[do_stats$condition == 12] and do_stats$pct_remain[do_stats$condition == 22]
#> D = 0.80769, p-value < 2.2e-16
#> alternative hypothesis: two-sided
#> 

```
