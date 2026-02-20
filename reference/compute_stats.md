# Compute Dropout Statistics

This is the *second step* in conducting dropout analysis with `dropR`.
Outputs all necessary statistics to analyze and visualize dropout, such
as the sample size N of the data (and in each condition if selected),
cumulative dropout and remaining participants in absolute numbers and
percent. If no experimental condition is added, the stats are only
calculated for the whole data in total.

## Usage

``` r
compute_stats(df, by_cond = "None", no_of_vars, excl_cond_NA = T)
```

## Arguments

- df:

  data.frame containing variable `do_idx` from
  [`add_dropout_idx()`](https://iscience-kn.github.io/dropR/reference/add_dropout_idx.md)

- by_cond:

  character name of condition variable in the data, defaults to 'None'
  to output total statistics.

- no_of_vars:

  numeric number of variables that contain questions

- excl_cond_NA:

  boolean Exclude NAs in the condition variable? This typically means
  that there is an entirely empty row in your data, i.e. someone quit
  before being assigned an experimental condition. This is not
  technically dropout and will skew results. Default is `TRUE`.

## Value

A data frame with 6 columns (q_idx, condition, cs, N, remain,
pct_remain) and as many rows as questions in original data (for overall
data and if conditions selected again for each condition).

## Examples

``` r
do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)
  
```
