# Dropout Odds Ratio Table

This function calculates an Odds Ratio table at a given question for
selected experimental conditions. It needs data in the format as created
by
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md)
as input.

## Usage

``` r
do_or_table(do_stats, chisq_question, sel_cond_chisq)
```

## Arguments

- do_stats:

  data.frame statistics table as computed by
  [`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).

- chisq_question:

  numeric Which question to calculate the OR table for

- sel_cond_chisq:

  character vector naming the experimental conditions to compare

## Value

Returns a Matrix containing the Odds Ratios of dropout between all
selected conditions.

## See also

[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md)

## Examples

``` r
do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)

do_or_table(do_stats, chisq_question = 51, sel_cond_chisq = c("11", "12", "21", "22"))
#>           11        12       21       22
#> 11 1.0000000 0.5426357 1.161616 1.510417
#> 12 1.8428571 1.0000000 2.140693 2.783482
#> 21 0.8608696 0.4671385 1.000000 1.300272
#> 22 0.6620690 0.3592622 0.769070 1.000000

```
