# https://adventofcode.com/2023/day/11

# init --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
# "." - empty space; "#" - galaxy; 
# Any rows or columns that contain no galaxies should all actually be twice as big.
# Find sum of the lengths of the shortest path between every pair of galaxies.
# 36 pairs, total sum 374
x_ <- read_lines(
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
")
# viz helper
cat_m_lgl <- \(m) apply(+m, 1, paste0, collapse ="") |> paste0(collapse ="\n") |> cat()

# boolean matrix, TRUE marks galaxy locations
x_ |>
  str_split("") |>
  do.call(what = rbind) -> m_str
m <- m_str == "#"  
cat_m_lgl(m)
#> 0001000000
#> 0000000100
#> 1000000000
#> 0000000000
#> 0000001000
#> 0100000000
#> 0000000001
#> 0000000000
#> 0000000100
#> 1000100000

# replication vectors for rows and cols
row_rep <- tibble(idx = seq(dim(m)[1]), n = (rowSums(m) == 0) + 1) |> uncount(n)
row_rep$idx
#>  [1]  1  2  3  4  4  5  6  7  8  8  9 10
col_rep <- tibble(idx = seq(dim(m)[2]), n = (colSums(m) == 0) + 1) |> uncount(n)
col_rep$idx
#>  [1]  1  2  3  3  4  5  6  6  7  8  9  9 10

# expand input with repeated indices 
m_expanded <- m[row_rep$idx,col_rep$idx]
cat_m_lgl(m_expanded)
#> 0000100000000
#> 0000000001000
#> 1000000000000
#> 0000000000000
#> 0000000000000
#> 0000000010000
#> 0100000000000
#> 0000000000001
#> 0000000000000
#> 0000000000000
#> 0000000001000
#> 1000010000000

# find galaxy locations as array indeces (matrix; a coordinate pair for every match);
# split matrix by rows (9 matches in the example), 
# generate all combinations of list elements(9), 2 at a time (36), 2x36 matrix of lists;
# transpose to 36x2 for tibble; get Manhattan distance between coordinate pairs
which(m_expanded, arr.ind = TRUE) |> 
  asplit(1) |>
  combn(2) |>
  t() |>
  `dimnames<-`(list(NULL, c("a", "b"))) |>
  as_tibble() |>
  mutate(manhattan_dist = map2_int(a, b, \(a, b) abs(a - b) |> sum())) %T>% 
  { print(head(as.data.frame(.))) } |>
  pull(manhattan_dist) |> 
  sum()
#>      a     b manhattan_dist
#> 1 3, 1 12, 1              9
#> 2 3, 1  7, 2              5
#> 3 3, 1  1, 5              6
#> 4 3, 1 12, 6             14
#> 5 3, 1  6, 9             11
#> 6 3, 1 2, 10             10
#> [1] 374

# 2 -----------------------------------------------------------------------
# Expansion factor for empty lines is 1000000 instead of 2
# For example data the factor is 100 and total sum is 8410

expand_by = 100

# collect expandable rows and columns 
(expanded_rows <- (rowSums(m) == 0) |> which())
#> [1] 4 8
(expanded_cols <- (colSums(m) == 0) |> which())
#> [1] 3 6 9

# Manhattan distance between every galaxy pair in input matrix, NOT expanded;
# between every coordinate pair, count number of rows and columns that would expand;
# adjust distance
which(m, arr.ind = TRUE) |> 
  asplit(1) |>
  combn(2) |> 
  t() |>
  `dimnames<-`(list(NULL, c("a", "b"))) |>
  as_tibble() |> 
  mutate(manhattan_dist = map2_int(a, b, \(a, b) abs(a - b) |> sum())) |> 
  rowwise() |> 
  mutate(
    exp_fact_rows = sum(expanded_rows %in% a[[1]]:b[[1]]),
    exp_fact_cols = sum(expanded_cols %in% a[[2]]:b[[2]])) |>
  ungroup() |>
  mutate(dist_expanded = manhattan_dist - (exp_fact_rows + exp_fact_cols) + (exp_fact_rows + exp_fact_cols) * expand_by) %T>% 
  { print(head(as.data.frame(.))) } |>
  pull(dist_expanded) |>
  sum() 
#>      a     b manhattan_dist exp_fact_rows exp_fact_cols dist_expanded
#> 1 3, 1 10, 1              7             2             0           205
#> 2 3, 1  6, 2              4             1             0           103
#> 3 3, 1  1, 4              5             0             1           104
#> 4 3, 1 10, 5             11             2             1           308
#> 5 3, 1  5, 7              8             1             2           305
#> 6 3, 1  2, 8              8             0             2           206
#> [1] 8410
