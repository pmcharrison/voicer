#' @export
voicer_cost_funs <- function(
  memoise = TRUE,
  vl_dist_norm = "taxicab",
  top = 72,
  middle = 60,
  bottom = 48
) {
  list(
    vl_dist = cf_vl_dist(memoise, vl_dist_norm),
    hutch_78 = cf_hutch_78(memoise),
    melody_dist = cf_melody_dist(),
    outer_parallels = cf_outer_parallels(),
    dist_above_top = cf_dist_above_top(top),
    dist_from_middle = cf_dist_from_middle(middle),
    dist_below_bottom = cf_dist_below_bottom(bottom)
  )
}

# part overlap rule?

# exposed octaves and fifths

# changes in the number of chord notes

