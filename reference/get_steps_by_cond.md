# Get Steps Data by Condition

The `get_steps_by_cond` function calculates steps data based on survival
model results. This utility function is used inside the
[`do_kpm()`](https://iscience-kn.github.io/dropR/reference/do_kpm.md)
function of `dropR`.

## Usage

``` r
get_steps_by_cond(sfit, condition = NULL)
```

## Arguments

- sfit:

  An object representing survival model results (e.g., from a
  Kaplan-Meier model).

- condition:

  Optional. An experimental condition to include in the output data
  frame, defaults to `NULL`.

## Value

Returns a data frame containing the steps data, including time, survival
estimates, upper confidence bounds, and lower confidence bounds.

## See also

[`do_kpm()`](https://iscience-kn.github.io/dropR/reference/do_kpm.md)
