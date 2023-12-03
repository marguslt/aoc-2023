# https://adventofcode.com/2023/day/3

# init --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
# Add up all the part numbers. Any number adjacent to a symbol, 
# even diagonally, is a "part number" and should be included in sum. 
# 114 (top right) and 58 (middle right) are not part numbers, 
# every other number is, sums up to 4361
x_ <- read_lines(
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

# collect all numbers with line / start / end
x_ |> str_match_all("\\d+") |> unlist() -> num_matches
x_ |> 
  str_locate_all("\\d+") |> 
  imap(\(x, idx) as_tibble(x) |> mutate(line = idx, .before = 1)) |>
  bind_rows() |>
  mutate(num =  as.numeric(num_matches), .before = 1) -> pn_tbl

pn_tbl
#> # A tibble: 10 × 4
#>      num  line start   end
#>    <dbl> <int> <int> <int>
#>  1   467     1     1     3
#>  2   114     1     6     8
#>  3    35     3     3     4
#>  4   633     3     7     9
#>  5   617     5     1     3
#>  6    58     6     8     9
#>  7   592     7     3     5
#>  8   755     8     7     9
#>  9   664    10     2     4
#> 10   598    10     6     8

# collect all symbols with line / loc
x_ |> str_match_all("[^\\d\\.]") |> unlist() -> sym_matches
x_ |> 
  str_locate_all("([^\\d\\.])") |> 
  imap(\(x, idx) as_tibble(x) |> mutate(line = idx, .before = 1) |> select(line, loc = start)) |>
  bind_rows() |>
  mutate(sym =  sym_matches, .before = 1) -> sym_tbl

sym_tbl
#> # A tibble: 6 × 3
#>   sym    line   loc
#>   <chr> <int> <int>
#> 1 *         2     4
#> 2 #         4     7
#> 3 *         5     4
#> 4 +         6     6
#> 5 $         9     4
#> 6 *         9     6

# inner_join by between() & overlaps(), keeps only numbers adjacent to a symbol
pn_sym_join <-
  sym_tbl |>
  mutate(line_min = line - 1,
         line_max = line + 1,
         start = loc - 1,
         end = loc + 1) |>
  inner_join(x = pn_tbl, y = _, by = join_by(between(x$line, y$line_min, y$line_max),
                                             overlaps(x$start, x$end, y$start, y$end)))
pn_sym_join
#> # A tibble: 8 × 11
#>     num line.x start.x end.x sym   line.y   loc line_min line_max start.y end.y
#>   <dbl>  <int>   <int> <int> <chr>  <int> <int>    <dbl>    <dbl>   <dbl> <dbl>
#> 1   467      1       1     3 *          2     4        1        3       3     5
#> 2    35      3       3     4 *          2     4        1        3       3     5
#> 3   633      3       7     9 #          4     7        3        5       6     8
#> 4   617      5       1     3 *          5     4        4        6       3     5
#> 5   592      7       3     5 +          6     6        5        7       5     7
#> 6   755      8       7     9 *          9     6        8       10       5     7
#> 7   664     10       2     4 $          9     4        8       10       3     5
#> 8   598     10       6     8 *          9     6        8       10       5     7

sum(pn_sym_join$num)
#> [1] 4361

# 2 -----------------------------------------------------------------------
# A gear is any * symbol that is adjacent to exactly two part numbers. 
# Its gear ratio is the result of multiplying those two numbers together.
# Find the gear ratio of every gear and add them all up
# Example: two gears. 1st with part numbers 467 and 35 (ratio 16345); 
# 2nd with 755, 598, ratio 451490; sum: 467835

# count all part numbers adjacent to a specific symbol at line/loc,
# keep only "*" symbols that are adjacent to exactly 2 PNs, 
# sum PN group products (gear_ratios)
pn_sym_join |>
  group_by(line.y, loc, sym) |>
  add_tally(name = "adj_count") |> 
  filter(sym == "*", adj_count == 2) %T>% print() |>
  summarise(gear_ratio = prod(num)) %T>% print() |>
  ungroup() |>
  pull(gear_ratio) |>
  sum()

#> # A tibble: 4 × 12
#> # Groups:   line.y, loc, sym [2]
#>     num line.x start.x end.x sym   line.y   loc line_min line_max start.y end.y
#>   <dbl>  <int>   <int> <int> <chr>  <int> <int>    <dbl>    <dbl>   <dbl> <dbl>
#> 1   467      1       1     3 *          2     4        1        3       3     5
#> 2    35      3       3     4 *          2     4        1        3       3     5
#> 3   755      8       7     9 *          9     6        8       10       5     7
#> 4   598     10       6     8 *          9     6        8       10       5     7
#> # ℹ 1 more variable: adj_count <int>
#> `summarise()` has grouped output by 'line.y', 'loc'. You can override using the
#> `.groups` argument.

#> # A tibble: 2 × 4
#> # Groups:   line.y, loc [2]
#>   line.y   loc sym   gear_ratio
#>    <int> <int> <chr>      <dbl>
#> 1      2     4 *          16345
#> 2      9     6 *         451490
#> [1] 467835

# -------------------------------------------------------------------------
# 1st (working) approach 
# Part 1:

# check if line/start:end is adjecent to a symbol, use with pmap
is_adjecent_to_symbol <- function(line, start, end, sym_tbl_ = sym_tbl){
  # lines do not have to exist in sym_tbl (0 and max + 1 are fine)
  sym_lines <- (line-1):(line+1)
  sym_locs  <- (start-1):(end+1)
  nrow(filter(sym_tbl_, line %in% sym_lines & 
                loc %in% sym_locs)) > 0
}

pn_tbl |>
  mutate(
    valid_pn = pmap_lgl(pick(line:end), is_adjecent_to_symbol)) %T>% print() |>
  filter(valid_pn) |>
  pull(num) |>
  sum()

#> # A tibble: 10 × 5
#>      num  line start   end valid_pn
#>    <dbl> <int> <int> <int> <lgl>   
#>  1   467     1     1     3 TRUE    
#>  2   114     1     6     8 FALSE   
#>  3    35     3     3     4 TRUE    
#>  4   633     3     7     9 TRUE    
#>  5   617     5     1     3 TRUE    
#>  6    58     6     8     9 FALSE   
#>  7   592     7     3     5 TRUE    
#>  8   755     8     7     9 TRUE    
#>  9   664    10     2     4 TRUE    
#> 10   598    10     6     8 TRUE
#> [1] 4361


# Part 2:
# get a vector of adjacent partnumbers, use with pmap
adj_pns <- function(line, loc, pn_tbl_ = pn_tbl){
  num_lines <- (line-1):(line+1)
  num_locs <- (loc-1):(loc+1)
  filter(pn_tbl_, line %in% num_lines) |>
    rowwise() |>
    filter(length(intersect(start:end, num_locs)) > 0) |>
    pull(num)
}

sym_tbl |>
  filter(sym == "*") |>
  mutate(adj_pns = pmap(pick(line:loc), adj_pns)) |>
  filter(lengths(adj_pns) == 2) |>
  mutate(gear_ratio = map_dbl(adj_pns, prod)) %T>% {print(as.data.frame(.))} |>
  pull(gear_ratio) |>
  sum()

#>   sym line loc  adj_pns gear_ratio
#> 1   *    2   4  467, 35      16345
#> 2   *    9   6 755, 598     451490
#> [1] 467835


  



