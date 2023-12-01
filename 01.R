# https://adventofcode.com/2023/day/1

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())

# 1 -----------------------------------------------------------------------
# Calibration values of these four lines are 12, 38, 15, and 77. 
# Adding these together produces 142
x_ <- read_lines(
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
")
x |>
  str_remove_all("[^\\d]") |>
  str_split("") |>
  map(\(l) str_c(head(l ,1), tail(l, 1))) |>
  map_int(as.numeric) |>
  sum()

# 2 -----------------------------------------------------------------------
# Calibration values are 29, 83, 13, 24, 42, 14, and 76. 
# Adding these together produces 281
x_ <- read_lines(
"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
")

num <- setNames(as.character(1:9), 
                c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")) 
x |>
  str_replace_all("(one|two|three|four|five|six|seven|eight|nine)", "\\1|") |>
  str_replace_all(num) |>
  str_remove_all("[^\\d]") |>
  str_split("") |>
  map(\(l) str_c(head(l ,1), tail(l, 1))) |>
  map_int(as.numeric) |>
  sum()

# -------------------------------------------------------------------------
