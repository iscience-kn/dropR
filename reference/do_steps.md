# Calculate Steps for Uneven Data Points

The `do_steps` function calculates steps for data points represented by
numbers of questions from the original experimental or survey data in
`x` and remaining percent of participants in `y`.

## Usage

``` r
do_steps(x, y, return_df = TRUE)
```

## Arguments

- x:

  Numeric vector representing the question numbers

- y:

  Numeric vector representing the remaining percent of participants

- return_df:

  Logical. If TRUE, the function returns a data frame; otherwise, it
  returns a list.

## Value

Returns a data frame or a list containing the modified `x` and `y`
values.

## Details

Due to the nature of dropout/ survival data, step functions are
necessary to accurately depict participants remaining. Dropout data
includes the time until the event (a.k.a. dropout at a certain question
or time), so that changes in remaining participants are discrete rather
than continuous. This means that changes in survival probability occur
at specific points and are better represented as steps than as a
continuum.

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
y <- c(100, 100, 95, 90, 85)
do_steps(x, y)
#>   x   y
#> 1 1 100
#> 2 3 100
#> 3 3  95
#> 4 4  95
#> 5 4  90
#> 6 5  90
#> 7 5  85

# Using the example dataset dropRdemo

do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
by_cond = "experimental_condition",
no_of_vars = 52)

tot_stats <- do_stats[do_stats$condition == "total", ]
do_steps(tot_stats$q_idx, tot_stats$pct_remain)
#>     x         y
#> 1   1 1.0000000
#> 2   2 1.0000000
#> 3   2 0.9593496
#> 4   3 0.9593496
#> 5   3 0.9471545
#> 6   4 0.9471545
#> 7   4 0.9105691
#> 8   5 0.9105691
#> 9   5 0.8983740
#> 10  6 0.8983740
#> 11  6 0.8943089
#> 12  7 0.8943089
#> 13  7 0.8780488
#> 14  8 0.8780488
#> 15  8 0.8617886
#> 16  9 0.8617886
#> 17  9 0.8536585
#> 18 10 0.8536585
#> 19 10 0.8495935
#> 20 11 0.8495935
#> 21 11 0.8333333
#> 22 12 0.8333333
#> 23 12 0.8292683
#> 24 13 0.8292683
#> 25 13 0.8008130
#> 26 16 0.8008130
#> 27 16 0.7926829
#> 28 17 0.7926829
#> 29 17 0.7886179
#> 30 18 0.7886179
#> 31 18 0.7845528
#> 32 19 0.7845528
#> 33 19 0.7723577
#> 34 20 0.7723577
#> 35 20 0.7642276
#> 36 21 0.7642276
#> 37 21 0.7601626
#> 38 22 0.7601626
#> 39 22 0.7520325
#> 40 23 0.7520325
#> 41 23 0.7439024
#> 42 24 0.7439024
#> 43 24 0.7357724
#> 44 26 0.7357724
#> 45 26 0.7276423
#> 46 27 0.7276423
#> 47 27 0.7235772
#> 48 28 0.7235772
#> 49 28 0.7195122
#> 50 29 0.7195122
#> 51 29 0.7154472
#> 52 30 0.7154472
#> 53 30 0.7113821
#> 54 31 0.7113821
#> 55 31 0.7073171
#> 56 32 0.7073171
#> 57 32 0.7032520
#> 58 33 0.7032520
#> 59 33 0.6991870
#> 60 34 0.6991870
#> 61 34 0.6951220
#> 62 35 0.6951220
#> 63 35 0.6910569
#> 64 36 0.6910569
#> 65 36 0.6788618
#> 66 37 0.6788618
#> 67 37 0.6707317
#> 68 38 0.6707317
#> 69 38 0.6666667
#> 70 40 0.6666667
#> 71 40 0.6626016
#> 72 41 0.6626016
#> 73 41 0.6544715
#> 74 45 0.6544715
#> 75 45 0.6504065
#> 76 46 0.6504065
#> 77 46 0.6422764
#> 78 47 0.6422764
#> 79 47 0.6341463
#> 80 48 0.6341463
#> 81 48 0.6260163
#> 82 49 0.6260163
#> 83 49 0.6219512
#> 84 52 0.6219512
#> 85 52 0.6178862
```
