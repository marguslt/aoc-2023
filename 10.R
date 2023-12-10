# https://adventofcode.com/2023/day/10

# init --------------------------------------------------------------------
library(tidyverse)
library(tidygraph)
library(igraph)
library(sf)
library(sfheaders)
source("aoc.R")
x <- read_lines(get_aoc("2023", "10"))

# 1 -----------------------------------------------------------------------
# A closed loop maze; how many steps along the loop does it take to get from 
# the starting position to the point farthest from the starting position?

x_ <- read_lines(
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
")
x_ |>
  str_split("") |>
  do.call(what = rbind) -> m

# create a safety zone around matrix
h_pad <- rep(".", dim(m)[1])
m <- cbind(x = h_pad, m, x = h_pad)
v_pad <- rep(".", dim(m)[2])
m <- rbind(x = v_pad, m, x = v_pad)
apply(m, 1, paste0, collapse ="") |> paste0(collapse ="\n") |> cat()
#> ......................
#> .FF7FSF7F7F7F7F7F---7.
#> .L|LJ||||||||||||F--J.
#> .FL-7LJLJ||||||LJL-77.
#> .F--JF--7||LJLJ7F7FJ-.
#> .L---JF-JLJ.||-FJLJJ7.
#> .|F|F-JF---7F7-L7L|7|.
#> .|FFJF7L7F-JF7|JL---7.
#> .7-L-JL7||F7|L7F-7F7|.
#> .L.L7LFJ|||||FJL7||LJ.
#> .L7JLJL-JLJLJL--JLJ.L.
#> ......................


# create edge destinations from current index, based on a symbol
get_links <- function(idx, tile_symbol, m_dim){
  switch (tile_symbol,
          "|" = idx + c(-1,1),
          "-" = idx + c(-1,1)  * m_dim[1],
          "L" = c(idx - 1, idx + m_dim[1]),
          "J" = c(idx - 1, idx - m_dim[1]),
          "7" = c(idx + 1, idx - m_dim[1]),
          "F" = c(idx + 1, idx + m_dim[1])
  ) |> as.integer()
}

# node table with row/col locations
node_tbl <- tibble( 
  node_idx = str_which(m, "\\||-|L|J|7|F|S"),
  tile = m[node_idx],
  loc = arrayInd(node_idx, dim(m))) |>
  mutate(name = str_glue("{tile} {node_idx}"),
         row = loc[,1], col = loc[,2]) |>
  select(-loc)
node_tbl
#> # A tibble: 197 × 5
#>    node_idx tile  name     row   col
#>       <int> <chr> <glue> <int> <int>
#>  1       14 F     F 14       2     2
#>  2       15 L     L 15       3     2
#>  3       16 F     F 16       4     2
#>  4       17 F     F 17       5     2
#>  5       18 L     L 18       6     2
#>  6       19 |     | 19       7     2
#>  7       20 |     | 20       8     2
#>  8       21 7     7 21       9     2
#>  9       22 L     L 22      10     2
#> 10       23 L     L 23      11     2
#> # ℹ 187 more rows

# start location
start_idx <- filter(node_tbl, tile == "S" )$node_idx

edge_tbl <-
  node_tbl |>
  mutate(to = map2(node_idx, tile, \(n_,t_) get_links(n_, t_, dim(m)))) |>
  unnest_longer(to) |>
  select(from = node_idx, to) |>
  # remove edges not in a node_tbl
  semi_join(node_tbl, by = join_by(to == node_idx))

# append links FROM start
edge_tbl <- bind_rows(
  edge_tbl, 
  select(edge_tbl[edge_tbl$to == start_idx, ], from = to, to = from))

# tidygraph seems to struggle with integer id-s when they do not fill the whole
# space, hence build with igraph
g <- graph_from_data_frame(edge_tbl, directed = TRUE, vertices = node_tbl) |> 
  as_tbl_graph()

# g %N>% as_tibble() %>% View(title = "nodes")
# g %E>% as_tibble() %>% View(title = "edges")

# node table was created in away that all valid edges must be mutual, 
# use that for filtering; get a component that includes start S
g_sub <- g |>
  activate(edges) |> 
  filter(edge_is_mutual()) |>
  convert(to_subcomponent, .N()$tile == "S") 
  ## |> activate(nodes) |>
  # mutate(grp = group_components(), dgr = centrality_degree(mode = "total")) |> 
  # as_tibble() |> View()

# length of the shortest circle and ordered vertex ids of the circle
ordered_nodes <- girth(g_sub)$circle
# furthest point from Start is half of circle length
length(ordered_nodes) / 2
#> [1] 80

# 2 -----------------------------------------------------------------------
# How many tiles are enclosed by the loop?

# maze point coordinates
maze_points <- 
  g_sub |> 
  activate(nodes) |> 
  as_tibble() |>
  select(name, x = col, y = row)

# all possible points in a grid, filter to exclude points already part of the 
# maze loop; sfc object
grid_points_sfc <- 
  expand_grid(x = min(maze_points$x):max(maze_points$x),
              y = min(maze_points$y):max(maze_points$y)) |>
  # remove points that exist in maze_points
  anti_join(maze_points) |>
  sfc_point()

plot(grid_points_sfc)

# maze polygon, sfc; left join to first sort the coordinates in the order of 
# girth(g_sub)$circle for proper maze shape
maze_poly_sfc <- 
  left_join(tibble(name = names(ordered_nodes)),maze_points) |>
  select(x, y) |>
  sfc_polygon()
plot(maze_poly_sfc)

points_within <- st_within(grid_points_sfc, maze_poly_sfc, sparse = FALSE)
sum(points_within)
#> [1] 10

# -------------------------------------------------------------------------
ggplot() +
  geom_sf(data = maze_poly_sfc) +
  geom_sf(data = grid_points_sfc[points_within]) +
  theme_void()
