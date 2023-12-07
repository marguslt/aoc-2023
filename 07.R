# https://adventofcode.com/2023/day/7

# init --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
source("aoc.R")
x <- read_lines(get_aoc())
x_ <- x

# 1 -----------------------------------------------------------------------
# 7 ranked hand types for 1st level ordering, 13 ranked cards for  2nd level ordering
# winning = bid * final rank; result is sum of scores 
# 765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5 = 6440

x_ <- read_lines(
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")

# integer hand classifier, sortable, highest rank(1) with smallest value(5)
hand_type <- function(hand_str){
  # "32T3K"
  # 2 K T 3 
  # 1 1 1 2
  #        2    K    T    3 
  # sum(1000* 100*  10*   2) = 1112
  h_counts <- str_split(hand_str, "") |> table() |> sort()
  sum(h_counts * 10^rev(seq_along(h_counts)-1))
}

# test hand types
c("AAAAA", "AKKKK", "AAKKK", "AQKKK", "AAKKQ", "AAKQJ", "AKQJT") |>
  purrr::set_names() |> map(hand_type) |> str()
#> List of 7
#>  $ AAAAA: num 5
#>  $ AKKKK: num 14
#>  $ AAKKK: num 23
#>  $ AQKKK: num 113
#>  $ AAKKQ: num 122
#>  $ AAKQJ: num 1112
#>  $ AKQJT: num 11111

# recode hands for lexical ordering 
hand_sortable <- function(hand_str, rank_map = set_names(13:1, c("A", "K", "Q", "J", "T", 9:2))){
  cards <- str_split(hand_str, "")[[1]]
  str_c(LETTERS[rank_map[cards]], collapse = "")
}
hand_sortable("AKQJT")
#> [1] "MLKJI"

x_ |>
  read_table(col_names = c("hand", "score")) |>
  mutate(h_type = map_int(hand, hand_type),
         h_sort_coded = map_chr(hand, hand_sortable)) |>
  arrange(desc(h_type), h_sort_coded) |>
  mutate(win = score * row_number()) %T>% print() |> 
  pull(win) |>
  sum()
#> # A tibble: 5 × 5
#>   hand  score h_type h_sort_coded   win
#>   <chr> <dbl>  <int> <chr>        <dbl>
#> 1 32T3K   765   1112 BAIBL          765
#> 2 KTJJT   220    122 LIJJI          440
#> 3 KK677    28    122 LLEFF           84
#> 4 T55J5   684    113 IDDJD         2736
#> 5 QQQJA   483    113 KKKJM         2415
#> [1] 6440

# 2 -----------------------------------------------------------------------
# joker: card with a lowest rank for 2nd level ordering,
# replaces any other card in hand to get strongest type possible

(c_ranks_j <- set_names(13:1, c("A", "K", "Q", "T", 9:2, "J")))
#>  A  K  Q  T  9  8  7  6  5  4  3  2  J 
#> 13 12 11 10  9  8  7  6  5  4  3  2  1

# use Joker count to update hand classifer to strongest type possible 
apply_j <- function(h_type, j_count){
  #    h_type j_count
  #  1      5       5
  #  2     14       1
  #  3     14       4
  #  4     23       2
  #  5     23       3
  #  6    113       1
  #  7    113       3
  #  8    122       1
  #  9    122       2
  # 10   1112       1
  # 11   1112       2
  # 12  11111       1
  case_when(
    h_type == 5 & j_count == 5 ~ 5,
    h_type %in% c(14,23) & j_count > 0 ~ 5,
    h_type == 113 & j_count > 0 ~ 14,
    h_type == 122 & j_count == 1 ~ 23,
    h_type == 122 & j_count == 2 ~ 14,
    h_type == 1112 & j_count > 0 ~ 113,
    h_type == 11111 & j_count == 1 ~ 1112,
    .default = h_type)
}

x_ |>
  read_table(col_names = c("hand", "score")) |>
  mutate(h_type = map_int(hand, hand_type),
         j_count = str_count(hand, "J"),
         h_type_j = apply_j(h_type, j_count),
         h_sort_coded_j = map_chr(hand, hand_sortable, rank_map = c_ranks_j)) |>
  arrange(desc(h_type_j), h_sort_coded_j) |>
  mutate(win = score * row_number()) %T>% print() |>
  pull(win) |>
  sum()
#> # A tibble: 5 × 7
#>   hand  score h_type j_count h_type_j h_sort_coded_j   win
#>   <chr> <dbl>  <int>   <int>    <dbl> <chr>          <dbl>
#> 1 32T3K   765   1112       0     1112 CBJCL            765
#> 2 KK677    28    122       0      122 LLFGG             56
#> 3 T55J5   684    113       1       14 JEEAE           2052
#> 4 QQQJA   483    113       1       14 KKKAM           1932
#> 5 KTJJT   220    122       2       14 LJAAJ           1100
#> [1] 5905
