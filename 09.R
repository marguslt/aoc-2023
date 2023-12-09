# https://adventofcode.com/2023/day/9

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
# Each line in the report contains the history of a single value, 
# include a prediction of the next value in each history;
# Be aware of negative values in input!

x_ <- read_lines(
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
")

predict_next <- function(v){
  accumulate(seq_along(v), \(x,y) diff(x), .init = v) |>
    compact() |>
    reduce(\(x,y) last(x) + last(y), .dir = "backward")
}

x_ |>
  str_extract_all("[-\\d]+") |>
  map(as.numeric) |>
  map_int(predict_next) |> 
  glimpse() |>
  sum()
#>  int [1:3] 18 28 68
#> [1] 114

# 2 -----------------------------------------------------------------------
# extrapolate backwards

extrapolate_prev <- function(v){
  accumulate(seq_along(v), \(x,y) diff(x), .init = v) |>
    compact() |>
    reduce(\(x,y) first(x) - first(y), .dir = "backward")
}

x_ |>
  str_extract_all("[-\\d]+") |>
  map(as.numeric) |>
  map_int(extrapolate_prev) |> 
  glimpse() |>
  sum()
#>  int [1:3] -3 0 5
#> [1] 2

# concept testing ---------------------------------------------------------
x_t <- c(10, 13, 16, 21, 30, 45)

# forward, accumulate last(x) + last(y)
# input vector length defines max number of diff() iterations 
seq_along(x_t) |> 
  # create series of diff-s
  accumulate(\(x,y) diff(x), .init = x_t) |> glimpse() |>
  # drop empty vectors, numeric(0)
  compact() |> glimpse() |>
  # accumulate from bottom up, add last elements of n and n-1
  accumulate(\(x,y) last(x) + last(y), .dir = "backward") |> glimpse()
#> List of 7
#>  $ : num [1:6] 10 13 16 21 30 45
#>  $ : num [1:5] 3 3 5 9 15
#>  $ : num [1:4] 0 2 4 6
#>  $ : num [1:3] 2 2 2
#>  $ : num [1:2] 0 0
#>  $ : num 0
#>  $ : num(0) 
#> List of 6
#>  $ : num [1:6] 10 13 16 21 30 45
#>  $ : num [1:5] 3 3 5 9 15
#>  $ : num [1:4] 0 2 4 6
#>  $ : num [1:3] 2 2 2
#>  $ : num [1:2] 0 0
#>  $ : num 0
#>  num [1:6] 68 23 8 2 0 0

# backward: accumulate first(x) - first(y)
seq_along(x_t) |> 
  accumulate(\(x,y) diff(x), .init = x_t) |>
  compact() |> glimpse() |> 
  # accumulate from bottom up, add last elements of n and n-1
  accumulate(\(x,y) first(x) - first(y), .dir = "backward") |> glimpse()
#> List of 6
#>  $ : num [1:6] 10 13 16 21 30 45
#>  $ : num [1:5] 3 3 5 9 15
#>  $ : num [1:4] 0 2 4 6
#>  $ : num [1:3] 2 2 2
#>  $ : num [1:2] 0 0
#>  $ : num 0
#>  num [1:6] 5 5 -2 2 0 0
