# Compute Odds From Probabilities

Compute odds from probabilities. The function is vectorized and can
handle a vector of probabilities, e.g. remaining percent of participants
as calculated by
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).

## Usage

``` r
get_odds(p)
```

## Arguments

- p:

  vector of probabilities. May not be larger than 1 or smaller than
  zero.

## Value

Returns numerical vector of the same length as original input reflecting
the odds.

## Examples

``` r
get_odds(0.7)
#> [1] 2.333333
get_odds(c(0.7, 0.2))
#> [1] 2.333333 0.250000

```
