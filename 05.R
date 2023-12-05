# https://adventofcode.com/2023/day/5

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())
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
    # which return integer(0) for non-matches, change those to 0,
    # for the rest look up a matching offset value
    map_if(.p = \(x) identical(x,integer(0)), 
           .f = \(x) 0, 
           .else = \(x) map_$offset[x])|>
    unlist()
}
reduce(maps, \(src_, map_) src_ + get_offset(src_, map_ ), .init = seeds) |> min()
#> [1] 35


# test concept ------------------------------------------------------------
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
