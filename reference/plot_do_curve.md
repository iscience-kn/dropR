# Plot Dropout Curves

This functions uses `ggplot2`to create drop out curves. Please note that
you should use
[`add_dropout_idx()`](https://iscience-kn.github.io/dropR/reference/add_dropout_idx.md)
and
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md)
on your data before running this function as it needs a certain data
structure and variables to work properly.

## Usage

``` r
plot_do_curve(
  do_stats,
  linetypes = TRUE,
  stroke_width = 1,
  full_scale = TRUE,
  show_points = FALSE,
  show_confbands = FALSE,
  color_palette = "color_blind"
)
```

## Arguments

- do_stats:

  data.frame containing dropout statistics table computed by
  [`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).
  Make sure your do_stats table contains a q_idx column indexing all
  question-items sequentially.

- linetypes:

  boolean Should different line types be used? Defaults to TRUE.

- stroke_width:

  numeric stroke width, defaults to 1.

- full_scale:

  boolean Should y axis range from 0 to 100? Defaults to TRUE, FALSE
  cuts off at min percent remaining (\>0).

- show_points:

  boolean Should dropout curves show individual data points? Defaults to
  FALSE.

- show_confbands:

  boolean Should there be confidence bands added to the plot? Defaults
  to FALSE.

- color_palette:

  character indicating which color palette to use. Defaults to
  'color_blind', alternatively choose 'gray' or 'default' for the
  ggplot2 default colors.

## Value

Returns a `ggplot` object containing the dropout curve plot. Using the
Shiny App version of dropR, this plot can easily be downloaded in
different formats.

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

plot_do_curve(do_stats)

```
