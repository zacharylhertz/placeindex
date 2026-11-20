# Synthetic blocks on a grid of unit squares. st_make_grid() fills cells row
# by row from the bottom left, so in a grid `n_col` cells wide the cell in
# row r and column c (both counted from 1) is number (r - 1) * n_col + c.
grid_blocks <- function(place, group, total, n_col, n_row = 1, offset = c(0, 0)) {
  sf::st_sf(
    place = place,
    group = group,
    total = total,
    geometry = sf::st_make_grid(cellsize = 1, offset = offset, n = c(n_col, n_row))
  )
}

cluster_fixture <- function() {
  # Place "A" is a 5 x 5 grid. Each cell is group/total, top row first so the
  # layout reads like a map:
  #
  #   row 5   70/100  70/100   0/0    120/150 120/150    C C . D D
  #   row 4   20/100  20/100  20/100   20/100  20/100
  #   row 3   20/100  20/100  40/50    20/100  20/100        B
  #   row 2   80/100  80/100  20/100   20/100  20/100    A A
  #   row 1   80/100  80/100  20/100   20/100  20/100    A A
  #
  # Majority blocks form four rook-contiguous clusters:
  #   A: 4 blocks, pop 400, group 320 (share 0.80)
  #   B: 1 block,  pop  50, group  40 (touches A only at a corner)
  #   C: 2 blocks, pop 200, group 140 (share 0.70)
  #   D: 2 blocks, pop 300, group 240 (share 0.80)
  # The empty cell in row 5, column 3 separates C from D. Bridging through
  # it gives C + D: pop 500, group 380 (share 0.76).
  # Place totals: pop 400 + 50 + 200 + 300 + 15 * 100 = 2450,
  #               group 320 + 40 + 140 + 240 + 15 * 20 = 1040.
  a <- grid_blocks(
    place = "A",
    group = c(80, 80, 20, 20, 20,
              80, 80, 20, 20, 20,
              20, 20, 40, 20, 20,
              20, 20, 20, 20, 20,
              70, 70, 0, 120, 120),
    total = c(100, 100, 100, 100, 100,
              100, 100, 100, 100, 100,
              100, 100, 50, 100, 100,
              100, 100, 100, 100, 100,
              100, 100, 0, 150, 150),
    n_col = 5,
    n_row = 5
  )
  # Place "B" is two blocks just right of row 5. Its first block shares an
  # edge with cluster D but lies in another place, so they must not join.
  # B: pop 200, one cluster of pop 100 and group 100.
  b <- grid_blocks("B", group = c(100, 10), total = c(100, 100), n_col = 2,
                   offset = c(5, 4))
  # A majority block outside every place, sharing an edge with A's
  # bottom-right cell. It must be dropped.
  outside <- grid_blocks(NA_character_, group = 90, total = 100, n_col = 1,
                         offset = c(5, 0))
  rbind(a, b, outside)
}

measures_fixture <- function() {
  # Place A: g = (40, 30, 10, 0, 0), t = (50, 50, 50, 50, 0), so the rest is
  # r = (10, 20, 40, 50, 0), G = 80, R = 120, T = 200.
  #   group share    = 80 / 200 = 0.4
  #   dissimilarity  = 0.5 * (|40/80 - 10/120| + |30/80 - 20/120|
  #                         + |10/80 - 40/120| + |0 - 50/120|)
  #                  = 0.5 * (5/12 + 5/24 + 5/24 + 5/12) = 0.625
  #   isolation      = (40/80)(40/50) + (30/80)(30/50) + (10/80)(10/50)
  #                  = 0.4 + 0.225 + 0.025 = 0.65
  #   exposure       = (40/80)(10/50) + (30/80)(20/50) + (10/80)(40/50)
  #                  = 0.1 + 0.15 + 0.1 = 0.35
  #   majority share = blocks with g / t > 0.5 are the first two:
  #                    (40 + 30) / 80 = 0.875
  #   The fifth block is empty and must not affect anything.
  # Place B: two identical half-group blocks. Dissimilarity 0, isolation and
  #   exposure 0.5, and majority share 0 because 0.5 is not above half.
  # Place C: no group members, so every measure that divides by G is NA.
  # The last block has no place and must be dropped.
  data.frame(
    place = c(rep("A", 5), "B", "B", "C", "C", NA),
    group = c(40, 30, 10, 0, 0, 10, 10, 0, 0, 5),
    total = c(50, 50, 50, 50, 0, 20, 20, 10, 5, 5)
  )
}

indicator_fixture <- function() {
  data.frame(
    unit = c("w", "x", "y", "z"),
    a = c(1, 2, 3, 6),
    b = c(4, 1, 2, 8)
  )
}
