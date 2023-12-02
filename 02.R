# Day 2: Cube Conundrum
# https://adventofcode.com/2023/day/2

# init --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
source("aoc.R")
x <- read_delim(get_aoc(), ": ", col_names = c("game", "cubes"))

# 1 -----------------------------------------------------------------------
# Which games would have been possible if the bag contained only 
# 12 red cubes, 13 green cubes, and 14 blue cubes?
# Possible: 1, 2, and 5; impossible: 3 (20 red), 4 (15 blue)
# What is the sum of the IDs of those games? (1+2+5 = 8)

x_ <- read_delim(
"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", 
": ", col_names = c("game", "cubes"))

x_ |>
  mutate(game = parse_number(game)) |>
  separate_longer_delim(cubes, regex(";\\s*")) |>
  mutate(subset_id = row_number(), .by = game) |>
  separate_longer_delim(cubes, regex(",\\s*")) |>
  separate_wider_delim(cubes, regex("\\s+"), names = c("n", "col")) |>
  mutate(n = as.numeric(n)) |> 
  pivot_wider(names_from = col, values_from = n, values_fill = 0) |>
  mutate(possible = red <= 12 & green <= 13 & blue <= 14) ->
  game_tbl

game_tbl
#> # A tibble: 14 × 6
#>     game subset_id  blue   red green possible
#>    <dbl>     <int> <dbl> <dbl> <dbl> <lgl>   
#>  1     1         1     3     4     0 TRUE    
#>  2     1         2     6     1     2 TRUE    
#>  3     1         3     0     0     2 TRUE    
#>  4     2         1     1     0     2 TRUE    
#>  5     2         2     4     1     3 TRUE    
#>  6     2         3     1     0     1 TRUE    
#>  7     3         1     6    20     8 FALSE   
#>  8     3         2     5     4    13 TRUE    
#>  9     3         3     0     1     5 TRUE    
#> 10     4         1     6     3     1 TRUE    
#> 11     4         2     0     6     3 TRUE    
#> 12     4         3    15    14     3 FALSE   
#> 13     5         1     1     6     3 TRUE    
#> 14     5         2     2     1     2 TRUE

game_tbl |>
  summarise(possible = all(possible), .by = game) |>
  filter(possible) |>
  pull(game) |>
  sum()
#> [1] 8

# 2 -----------------------------------------------------------------------
# What is the fewest number of cubes of each color that could have been 
# in the bag to make the game possible?
# 1: the game could have been played with 4 red, 2 green, and 6 blue cubes
# 2: minimum of 1 red, 3 green, and 4 blue cubes
# What is the sum of the power of these sets? (48+12+1560+630+36 = 2286)
  
game_tbl |>
  summarise(across(c(red, green, blue), max, .names = "{.col}.max"), .by = game) |>
  mutate(pow = red.max * blue.max * green.max) %T>% print() |>
  pull(pow) |>
  sum()

#> # A tibble: 5 × 5
#>    game red.max green.max blue.max   pow
#>   <dbl>   <dbl>     <dbl>    <dbl> <dbl>
#> 1     1       4         2        6    48
#> 2     2       1         3        4    12
#> 3     3      20        13        6  1560
#> 4     4      14         3       15   630
#> 5     5       6         3        2    36
#> [1] 2286

# -------------------------------------------------------------------------
