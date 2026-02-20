# Add Dropout Index to a Data.Frame

Find drop out positions in a data.frame that contains multiple
questions/items that had been asked/worked on sequentially. This
function adds the Dropout Index variable `do_idx` to the data.frame
which is necessary for further analyses of dropout.

Use this function *first* to prepare your dropout analysis. Then, keep
going by creating the dropout statistics using
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).

## Usage

``` r
add_dropout_idx(df, q_pos)
```

## Source

R/add_dropout_idx.R

## Arguments

- df:

  data.frame containing `NA`s

- q_pos:

  numeric range of columns that contain question items

## Value

Returns original data frame with column `do_idx` added.

## Details

Importantly, this function will start counting missing data at the end
of the data frame. Any missing data which is somewhere in between, i.e.
a single item that was skipped or forgotten will not be counted as
dropout. The function will identify sequences of missing data that go
until the end of the data frame and add the number of the last answered
question in `do_idx`.

The Dropout Index variable `do_idx` will therefore code the last item in
sequence that is not NA. If every item is not NA (every item was
answered), the `do_idx` will be one longer than the number of items in
the analysis to ensure that this is not counted as dropout in further
analyses, e.g. in
[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md).

Therefore, the variables must be in the order that they were asked,
otherwise analyses will not be valid.

## See also

[`compute_stats()`](https://iscience-kn.github.io/dropR/reference/compute_stats.md)
which is usually the next step for dropout analysis.

## Examples

``` r
dropout <- add_dropout_idx(dropRdemo, 3:54)
```
