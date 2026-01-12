# Kaplan-Meier Survival Estimation

This function needs a data set with a dropout index added by
[`add_dropout_idx()`](https://iscience-kn.github.io/dropR/reference/add_dropout_idx.md).
The `do_kpm` function performs survival analysis with Kaplan-Meier
Estimation and returns a list containing survival steps, the original
data frame, and the model fit type. The function can fit the survival
model either for the entire data set or separately by a specified
condition column.

## Usage

``` r
do_kpm(df, condition_col = "experimental_condition", model_fit = "total")
```

## Arguments

- df:

  data set with `do_idx` added by
  [`add_dropout_idx()`](https://iscience-kn.github.io/dropR/reference/add_dropout_idx.md)

- condition_col:

  character denoting the experimental conditions to model

- model_fit:

  character Should be either "total" for a total model or "conditions"

## Value

Returns a list containing `steps` (survival steps extracted from the
fitted models), `d` (the original data frame), and `model_fit` (the
model fit type).

## See also

[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) used to
fit survival object.

## Examples

``` r
demo_kpm <- do_kpm(df = add_dropout_idx(dropRdemo, 3:54),
condition_col = "experimental_condition",
model_fit = "total")

head(demo_kpm$steps)
#>   x         y      uppr       lwr condition
#> 1 1 0.9593496 0.9843472 0.9349868     total
#> 2 2 0.9593496 0.9843472 0.9349868     total
#> 3 2 0.9471545 0.9755285 0.9196058     total
#> 4 3 0.9471545 0.9755285 0.9196058     total
#> 5 3 0.9105691 0.9469365 0.8755984     total
#> 6 4 0.9105691 0.9469365 0.8755984     total
```
