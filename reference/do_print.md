# Print dropout / test results in a compact, report-ready string

Print dropout / test results in a compact, report-ready string

## Usage

``` r
do_print(x, ..., as_markdown = FALSE, print = FALSE)

# S3 method for class 'do_stats'
do_print(
  x,
  as_markdown = FALSE,
  item = NULL,
  conditions = NULL,
  digits_pct = 1,
  ...
)

# S3 method for class 'do_chi'
do_print(x, as_markdown = FALSE, digits_pct = 1, digits_stat = 2, ...)
```

## Arguments

- x:

  An object of class do_chisq (from `do_chisq`).

- ...:

  Additional arguments.

- as_markdown:

  Boolean. Should the output be formatted for a Markdown document (e.g.
  Quarto)? Defaults to FALSE.

- print:

  Boolean. Should the output be formatted for printing (e.g. to store in
  a character vector)? Defaults to FALSE.

- item:

  Numeric item index. Default NULL = last item for do_stats.

- conditions:

  Optional vector of conditions to include (ignored for totals unless
  you include them).

- digits_pct:

  Digits for percentages.

- digits_stat:

  Digits for test statistic.

## Value

Summary of the input object as either a string, markdown-ready or
console output.

## Examples

``` r
do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)

do_print(do_stats)
#> [1] "dropout up to item 52: total=38.2%, 11=37.5%, 12=26.3%, 21=41.1%, 22=47.5%."
```
