# Compute Odds Ratio

Computes odds ratio given two probabilities. In this package, the
function can be used to compare the percentages of remaining
participants between two conditions at a time.

## Usage

``` r
get_odds_ratio(a, b)
```

## Arguments

- a:

  numeric probability value between 0 and 1.

- b:

  numeric probability value between 0 and 1.

## Value

Returns numerical vector of the same length as original input reflecting
the Odds Ratio (OR).

## See also

[`get_odds()`](https://iscience-kn.github.io/dropR/reference/get_odds.md),
as this is the basis for calculation.

## Examples

``` r
get_odds_ratio(0.7, 0.6)
#> [1] 1.555556
```
