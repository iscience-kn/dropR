# Compute Chi-Squared Test Given a Question Position

This function performs a chi-squared contingency table test on dropout
for a given question in the data. Note that the input data should be in
the format as computed by
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).
The test can be performed on either all conditions (excluding total) or
on select conditions.

## Usage

``` r
do_chisq(
  do_stats,
  chisq_question = max(unique(do_stats$q_idx)),
  sel_cond_chisq = NULL,
  p_sim = TRUE
)
```

## Arguments

- do_stats:

  data.frame of dropout statistics as computed by
  [`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).

- chisq_question:

  numeric Which question to compare dropout at.

- sel_cond_chisq:

  vector (same class as in conditions variable in original data set)
  selected conditions.

- p_sim:

  boolean Simulate p value parameter (by Monte Carlo simulation)?
  Defaults to `TRUE`.

## Value

Returns test results from chisq.test between experimental conditions at
defined question.

## See also

[`add_dropout_idx()`](https://iscience-kn.github.io/dropR/reference/add_dropout_idx.md)
and
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md)
which are necessary for the proper data structure.

## Examples

``` r
do_stats <- compute_stats(add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)

do_chisq(do_stats, 47, c(12, 22), TRUE)
#> $`Test result`
#> 
#>  Pearson's Chi-squared test with simulated p-value (based on 2000
#>  replicates)
#> 
#> data:  test_table
#> X-squared = 5.8536, df = NA, p-value = 0.02149
#> 
#> 
#> $`Dropout up to item 47`
#>           participants
#> conditions dropout remaining
#>         12      14        43
#>         22      28        33
#> 
#> attr(,"class")
#> [1] "do_chi" "list"  
```
