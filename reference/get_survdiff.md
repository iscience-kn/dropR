# Test Survival Curve Differences

This function compares survival curves as modeled with
[`do_kpm()`](https://iscience-kn.github.io/dropR/reference/do_kpm.md).
It outputs a contingency table and a Chisq measure of difference.

## Usage

``` r
get_survdiff(kds, cond, test_type)
```

## Arguments

- kds:

  data set of a survival model such as
  [`do_kpm()`](https://iscience-kn.github.io/dropR/reference/do_kpm.md)

- cond:

  character of experimental condition variable in the data

- test_type:

  numeric (0 or 1) parameter that controls the type of test (0 means rho
  = 0; log-rank, 1 means rho = 1; Peto & Peto Wilcox)

## Value

Returns survival test results as called from
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html).

## Examples

``` r
kpm_est <- do_kpm(add_dropout_idx(dropRdemo, 3:54))
get_survdiff(kpm_est$d, "experimental_condition", 0)
#> Call:
#> survdiff(formula = f, data = kds, rho = test_type)
#> 
#>                            N Observed Expected (O-E)^2/E (O-E)^2/V
#> experimental_condition=11 72       27     27.6    0.0148    0.0212
#> experimental_condition=12 57       15     23.6    3.1164    4.2203
#> experimental_condition=21 56       23     21.2    0.1505    0.1972
#> experimental_condition=22 61       29     21.6    2.5533    3.3664
#> 
#>  Chisq= 5.9  on 3 degrees of freedom, p= 0.1 
get_survdiff(kpm_est$d, "experimental_condition", 1)
#> Call:
#> survdiff(formula = f, data = kds, rho = test_type)
#> 
#>                            N Observed Expected (O-E)^2/E (O-E)^2/V
#> experimental_condition=11 72     22.1     22.6   0.00792    0.0137
#> experimental_condition=12 57     12.2     19.1   2.54844    4.1614
#> experimental_condition=21 56     18.7     17.3   0.10125    0.1596
#> experimental_condition=22 61     23.9     17.8   2.08041    3.2905
#> 
#>  Chisq= 5.8  on 3 degrees of freedom, p= 0.1 

```
