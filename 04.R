# https://adventofcode.com/2023/day/4

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
# Which of the numbers you have appear in the list of winning numbers. 
# The first match makes the card worth one point and each match after the first 
# doubles the point value of that card
# How many points are they worth in total? (8 + 2 + 1 + 0 + 0 = 13)
x_ <- read_lines(
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

x_ |> 
  tibble(x = _) |>
  separate_wider_delim(x, delim = regex("\\s*(:|\\|)\\s+"), names = c("card", "winning", "mine")) |>
  mutate(across(winning:mine, ~ str_split(., "\\s+")), 
         card = parse_number(card) |> as.integer(),
         my_wins = map2(winning, mine, intersect),
         my_wins_n = lengths(my_wins),
         points = if_else(my_wins_n > 2, 2**(my_wins_n - 1), my_wins_n)) -> cardpile

cardpile
#> # A tibble: 6 × 6
#>    card winning   mine      my_wins   my_wins_n points
#>   <int> <list>    <list>    <list>        <int>  <dbl>
#> 1     1 <chr [5]> <chr [8]> <chr [4]>         4      8
#> 2     2 <chr [5]> <chr [8]> <chr [2]>         2      2
#> 3     3 <chr [5]> <chr [8]> <chr [2]>         2      2
#> 4     4 <chr [5]> <chr [8]> <chr [1]>         1      1
#> 5     5 <chr [5]> <chr [8]> <chr [0]>         0      0
#> 6     6 <chr [5]> <chr [8]> <chr [0]>         0      0

glimpse(cardpile)
#> Rows: 6
#> Columns: 6
#> $ card      <int> 1, 2, 3, 4, 5, 6
#> $ winning   <list> <"41", "48", "83", "86", "17">, <"13", "32", "20", "16", "61…
#> $ mine      <list> <"83", "86", "6", "31", "17", "9", "48", "53">, <"61", "30"…
#> $ my_wins   <list> <"48", "83", "86", "17">, <"32", "61">, <"1", "21">, "84", …
#> $ my_wins_n <int> 4, 2, 2, 1, 0, 0
#> $ points    <dbl> 8, 2, 2, 1, 0, 0

sum(cardpile$points)
#> [1] 13

# 2 -----------------------------------------------------------------------
# You win copies of the scratchcards below the winning card equal to the number of matches
# How many total scratchcards do you end up with?

cardpile <- 
  cardpile |>
  rowwise() |>
  mutate(copies = if_else(my_wins_n > 0, list(card + seq(my_wins_n)) , NA)) |>
  ungroup()

select(cardpile, card, my_wins_n, copies) |> as.data.frame()
#>   card my_wins_n     copies
#> 1    1         4 2, 3, 4, 5
#> 2    2         2       3, 4
#> 3    3         2       4, 5
#> 4    4         1          5
#> 5    5         0       NULL
#> 6    6         0       NULL

cards <- cardpile$card
collect_cards <- list()
while(!is.null(cards)){
  collect_cards <- append(collect_cards, list(cards))
  cards <- cardpile$copies[cards] |> unlist()
}

unlist(collect_cards) |> length()
#> [1] 30

# Task 2 concept test -----------------------------------------------------

(n1 <- cardpile$card)
#> [1] 1 2 3 4 5 6
(n2 <- cardpile$copies[n1] |> unlist())
#> [1] 2 3 4 5 3 4 4 5 5
(n3 <- cardpile$copies[n2] |> unlist())
#> [1] 3 4 4 5 5 4 5 5 5
(n4 <- cardpile$copies[n3] |> unlist())
#> [1] 4 5 5 5 5
(n5 <- cardpile$copies[n4] |> unlist())
#> [1] 5
(n6 <- cardpile$copies[n5] |> unlist())
#> NULL

table(c(n1,n2,n3,n4,n5,n6))
#> 
#>  1  2  3  4  5  6 
#>  1  2  4  8 14  1
length(c(n1,n2,n3,n4,n5,n6))
#> [1] 30
