# https://adventofcode.com/2023/day/5

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc("2023", "05"))
x_ <- x
# 1 -----------------------------------------------------------------------
# maps: destination range start, source range start, range length
# source number that aren't mapped uncorrespond to the same destination number
# What is the lowest location number that corresponds to any of the initial seed numbers?
x_ <- read_lines(
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
")

seeds <- x_[1] |> str_extract_all("\\d+", simplify = TRUE) |> as.numeric()

maps <- x_[-(1:2)] |> 
  tibble(maps = _) |>
  mutate(map_seq = str_detect(maps, "map") |> cumsum(), .before = 1) |>
  mutate(s = str_extract(maps[1], "[^\\s]+"), .by = map_seq) |>
  filter(str_detect(maps, "\\d+")) |>
  separate_wider_delim(maps, regex("\\s+"), names = c("dst", "src", "len")) |>
  mutate(across(dst:len, as.numeric),
         offset = dst - src) |>
  select(-s) |>
  split(~map_seq)

maps[1:2]
#> $`1`
#> # A tibble: 2 × 5
#>   map_seq   dst   src   len offset
#>     <int> <dbl> <dbl> <dbl>  <dbl>
#> 1       1    50    98     2    -48
#> 2       1    52    50    48      2
#> 
#> $`2`
#> # A tibble: 3 × 5
#>   map_seq   dst   src   len offset
#>     <int> <dbl> <dbl> <dbl>  <dbl>
#> 1       2     0    15    37    -15
#> 2       2    37    52     2    -15
#> 3       2    39     0    15     39

# offset(s) for source vector
get_offset <- function(src_, map_){
  map(src_, \(x) between(rep(x,nrow(map_)), map_$src, map_$src + map_$len - 1)) |>
    map(which) |>
    # which returns integer(0) for non-matches, change those to 0,
    # for the rest look up a matching offset value
    map_if(.p = \(x) identical(x,integer(0)), 
           .f = \(x) 0, 
           .else = \(x) map_$offset[x])|>
    unlist()
}
reduce(maps, \(src_, map_) src_ + get_offset(src_, map_ ), .init = seeds) |> min()
#> [1] 35


# testing concept ------------------------------------------------------------
get_offset(seeds, maps[[1]])
#> [1] 2 0 2 0

seeds
#> [1] 79 14 55 13
(m1 <- seeds + get_offset(seeds, maps[[1]]))
#> [1] 81 14 57 13
(m2 <- seeds + get_offset(m1,    maps[[2]]))
#> [1] 79 53 55 52

accumulate(maps, \(src_, map_) src_ + get_offset(src_, map_ ), .init = seeds) |> str()
#> List of 8
#>  $ .init: num [1:4] 79 14 55 13
#>  $ 1    : num [1:4] 81 14 57 13
#>  $ 2    : num [1:4] 81 53 57 52
#>  $ 3    : num [1:4] 81 49 53 41
#>  $ 4    : num [1:4] 74 42 46 34
#>  $ 5    : num [1:4] 78 42 82 34
#>  $ 6    : num [1:4] 78 43 82 35
#>  $ 7    : num [1:4] 82 43 86 35

# 2 -----------------------------------------------------------------------
# seed inputs are seed ranges, start-length pairs
# "seeds: 79 14 55 13" : 79, 80, ..., 91, 92; 55, 56, ..., 66, 67
# What is the lowest location number that corresponds to any of the initial seed numbers?
# input data generates 3,008,511,937 seeds ... 


get_offset_bck <- function(result_, map_){
  map(result_, \(x) between(rep(x,nrow(map_)), map_$dst, map_$dst + map_$len - 1)) |>
    map(which) |>
    # which returns integer(0) for non-matches, change those to 0,
    # for the rest look up a matching offset value
    map_if(.p = \(x) identical(x,integer(0)), 
           .f = \(x) 0, 
           .else = \(x) map_$offset[x])|>
    unlist()
}

search_max <- 
  maps$`7` |>
  slice_min(dst) |>
  mutate(end = dst + len - 1) |>
  pull(end)

# sparse backwards scan though locations, locations -> seeds
locs <- seq(0, search_max, by = 50000)
seeds_backwd <- reduce(rev(maps), \(location_, map_) location_ - get_offset_bck(location_, map_ ), .init = locs)
# plot(seeds_backwd ~ locs)

seeds_2 <- 
  x_[1] |> 
  str_extract_all("\\d+", simplify = TRUE) |> 
  as.numeric() |>
  matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL,c("start", "len"))) |>
  as_tibble() |>
  mutate(end = start + len - 1) |>
  rowwise() |>
  mutate(locs_within_sparse = list(locs[seeds_backwd >= start & seeds_backwd <= end]),
         min_loc_sparse = min(locs_within_sparse),
         matching_seed_sparse = seeds_backwd[match(min_loc_sparse, locs)]) 
seeds_2
#> # A tibble: 10 × 6
#> # Rowwise: 
#>       start    len    end locs_within_sparse min_loc_sparse matching_seed_sparse
#>       <dbl>  <dbl>  <dbl> <list>                      <dbl>                <dbl>
#>  1   1.48e9 3.39e8 1.82e9 <dbl [952]>              47250000           1568717311
#>  2   3.21e9 5.12e8 3.72e9 <dbl [38]>              328100000           3236119796
#>  3   4.26e7 5.18e7 9.44e7 <dbl [120]>              13800000             69698780
#>  4   2.57e8 3.80e8 6.36e8 <dbl [0]>                     Inf                   NA
#>  5   3.04e9 1.40e8 3.18e9 <dbl [768]>              61900000           3084137113
#>  6   4.02e9 1.17e8 4.14e9 <dbl [377]>               5250000           4042263169
#>  7   2.89e9 8.95e7 2.98e9 <dbl [0]>                     Inf                   NA
#>  8   6.70e8 8.07e8 1.48e9 <dbl [796]>              19800000           1074157484
#>  9   2.37e9 4.90e8 2.86e9 <dbl [3,117]>           100300000           2800392538
#> 10   2.09e9 8.29e7 2.17e9 <dbl [0]>                     Inf                   NA


# dense scan to cover 50000 locations preceding min(min_loc_sparse)
target_loc <- min(seeds_2$min_loc_sparse)
locs <- seq(target_loc - 50000, target_loc)
seeds_backwd <- reduce(rev(maps), \(location_, map_) location_ - get_offset_bck(location_, map_ ), .init = locs)

seeds_2 |>
  mutate(locs_within_dense = list(locs[seeds_backwd >= start & seeds_backwd <= end]),
         min_loc_dense = min(locs_within_dense)) |>
  pull(min_loc_dense) |>
  min()
#> [1] 5200543
  

# testing and debugging backwards bruteforce -----------------------------
# generate a sample (some trial and error to get a section that would include  
# 0 or 1 for final location(7))
accumulate(maps, \(src_, map_) src_ + get_offset(src_, map_ ), .init = c(20:30)) |> 
  bind_rows()
#> # A tibble: 11 × 8
#>    .init   `1`   `2`   `3`   `4`   `5`   `6`   `7`
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1    20    20     5    47    40    40    41    41
#>  2    21    21     6    48    41    41    42    42
#>  3    22    22     7    57    50    86    86    90
#>  4    23    23     8    58    51    87    87    91
#>  5    24    24     9    59    52    88    88    92
#>  6    25    25    10    60    53    89    89    93
#>  7    26    26    11     0     0     0     1     1
#>  8    27    27    12     1     1     1     2     2
#>  9    28    28    13     2     2     2     3     3
#> 10    29    29    14     3     3     3     4     4
#> 11    30    30    15     4     4     4     5     5


# get from locations c(41:42, 90:93, 1:5) back to seeds c(20:30)
loc_7 <- c(41:42, 90:93, 1:5)
(loc_6 <- loc_7 - get_offset_bck(loc_7, maps[[7]]))
#>  [1] 41 42 86 87 88 89  1  2  3  4  5
(loc_5 <- loc_6 - get_offset_bck(loc_6, maps[[6]]))
#>  [1] 40 41 86 87 88 89  0  1  2  3  4
(loc_4 <- loc_5 - get_offset_bck(loc_5, maps[[5]]))
#>  [1] 40 41 50 51 52 53  0  1  2  3  4
(loc_3 <- loc_4 - get_offset_bck(loc_4, maps[[4]]))
#>  [1] 47 48 57 58 59 60  0  1  2  3  4
(loc_2 <- loc_3 - get_offset_bck(loc_3, maps[[3]]))
#>  [1]  5  6  7  8  9 10 11 12 13 14 15
(loc_1 <- loc_2 - get_offset_bck(loc_2, maps[[2]]))
#>  [1] 20 21 22 23 24 25 26 27 28 29 30
(xseed <- loc_1 - get_offset_bck(loc_1, maps[[1]]))
#>  [1] 20 21 22 23 24 25 26 27 28 29 30

# process reversed: reverse maps order and subtract offset from current locations
accumulate(rev(maps), \(location_, map_) location_ - get_offset_bck(location_, map_ ), .init = loc_7 ) |>
  bind_rows()
#> # A tibble: 11 × 8
#>    .init   `7`   `6`   `5`   `4`   `3`   `2`   `1`
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1    41    41    40    40    47     5    20    20
#>  2    42    42    41    41    48     6    21    21
#>  3    90    86    86    50    57     7    22    22
#>  4    91    87    87    51    58     8    23    23
#>  5    92    88    88    52    59     9    24    24
#>  6    93    89    89    53    60    10    25    25
#>  7     1     1     0     0     0    11    26    26
#>  8     2     2     1     1     1    12    27    27
#>  9     3     3     2     2     2    13    28    28
#> 10     4     4     3     3     3    14    29    29
#> 11     5     5     4     4     4    15    30    30
