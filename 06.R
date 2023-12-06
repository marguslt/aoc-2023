# https://adventofcode.com/2023/day/6

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
x_ <- read_lines(
"Time:      7  15   30
Distance:  9  40  200
") 

x_ |>
  str_extract_all("\\d+") |>
  set_names("time", "dist") |>
  map(as.numeric) |>
  do.call(what = cbind) |>
  as_tibble() |>
  rowwise() |>
  mutate(wins = sum(1:(time-1) * (time-1):1 > dist)) |>
  pull(wins) |>
  prod()
#> [1] 288

# 2 -----------------------------------------------------------------------
x |>
  str_extract_all("\\d+") |>
  map(str_c, collapse = "") |>
  unlist() |>
  as.numeric() -> race
race
#> [1]  71530 940200

# as.numeric(): with "integer" storage mode actual dataset generates 
# integer overflow
a <- 1:(race[1]-1) |> as.numeric()
b <- rev(a)
sum((a * b) > race[2])
#> [1] 71503

# -------------------------------------------------------------------------
