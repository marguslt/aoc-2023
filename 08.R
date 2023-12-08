
setwd(r"(C:/Users/marguslt/_lab/r/aoc-2023)")
# https://adventofcode.com/2023/day/8

# init --------------------------------------------------------------------
library(tidyverse)
source("aoc.R")
x <- read_lines(get_aoc())
x_ <- x
# 1 -----------------------------------------------------------------------
# Starting with AAA, you need to look up the next element based on the next 
# left/right instruction in your input.
# Starting at AAA,how many steps are required to reach ZZZ?
x_ <- read_lines(
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
")

(instr_lr <- x_[1] |> str_split_1(""))
#> [1] "L" "L" "R"

x_[-1] |>
  str_extract_all("\\w+") |>
  do.call(what = rbind) -> map_matrix

mm2 <- map_matrix[,2:3] 
dimnames(mm2) <- list(map_matrix[,1], c("L", "R"))
mm2
#>     L     R    
#> AAA "BBB" "BBB"
#> BBB "AAA" "ZZZ"
#> ZZZ "ZZZ" "ZZZ"

idx <- 0L
instr_loop <- 0L
loc  <- "AAA"
dest <- "ZZZ"

while (loc != dest) {
  if(idx == length(instr_lr)){
    idx <- 1L
    instr_loop <- instr_loop + 1L
  } else {
    idx <- idx + 1L
  }
  
  message(str_glue("{loc} : ({str_c(mm2[loc,], collapse = ' ')}) "), appendLF = FALSE)
  message(str_glue("{instr_lr[idx]} -> "), appendLF = FALSE)
  # look up l/r instruction from instr_lr to look up a new location from mm2 matrix
  loc <- mm2[loc,instr_lr[idx]]
  message(loc)
}
#> AAA : (BBB BBB) L -> BBB
#> BBB : (AAA ZZZ) L -> AAA
#> AAA : (BBB BBB) R -> BBB
#> BBB : (AAA ZZZ) L -> AAA
#> AAA : (BBB BBB) L -> BBB
#> BBB : (AAA ZZZ) R -> ZZZ
instr_loop * length(instr_lr) + idx
#> [1] 6

# 2 -----------------------------------------------------------------------
# Simultaneously start on every node that ends with A. 
# How many steps does it take before you're only on nodes that end with Z?

# had to take a hint on LCM
x_ <- x

(instr_lr <- x_[1] |> str_split_1(""))
#>   [1] "L" "L" "L" "R" "R" "L" "R" "R" "R" "L" "L" "R" "R" "L" "R" "R" "L" ...

x_[-1] |>
  str_extract_all("\\w+") |>
  do.call(what = rbind) -> map_matrix

mm2 <- map_matrix[,2:3] 
dimnames(mm2) <- list(map_matrix[,1], c("L", "R"))
head(mm2)
#>     L     R    
#> FSH "CGN" "NDK"
#> LQT "NSK" "XBG"
#> LCP "QQB" "NTB"
#> DFG "KTV" "NJR"
#> MCC "TRF" "NHH"
#> PHG "VMX" "SHB"

idx <- 0L
instr_loop <- 0L

# all starting locations
(locs <- rownames(mm2) |> str_subset("A$"))
#> [1] "AAA" "NJA" "BHA" "HTA" "LJA" "XXA"

# all destinations
(destinations <- rownames(mm2) |> str_subset("Z$"))
#> [1] "HXZ" "GHZ" "TPZ" "FQZ" "PVZ" "ZZZ"

# step counts for all start positions
steps <- seq_along(locs) * 0

# all paths are following the same set of instructions;
# use a locations vector to handle all paths in one go
while (any(steps == 0)) {
  # when any of the destinations is reached, record current number of steps
  finished <- locs %in% destinations
  if (any(finished)){
    steps[finished] <- instr_loop * length(instr_lr) + idx
    print(steps)
  }
  if(idx == length(instr_lr)){
    idx <- 1L
    instr_loop <- instr_loop + 1L
  } else {
    idx <- idx + 1L
  }
  # retrieve length(locs) number of locations from lookup matrix
  locs <- mm2[locs,instr_lr[idx]]
}
#> [1]     0     0 11911     0     0     0
#> [1]     0 13019 11911     0     0     0
#> [1]     0 13019 11911 16897     0     0
#> [1]     0 13019 11911 16897     0 18559
#> [1]     0 13019 11911 16897 19667 18559
#> [1] 21883 13019 11911 16897 19667 18559
lcm <- reduce(steps, pracma::Lcm)
sprintf("%.f", lcm)
#> [1] "12833235391111"
