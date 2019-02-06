#' @export
voicer_cost_funs <- function(
  vl_dist_norm = "taxicab",
  top = 72,
  middle = 60,
  bottom = 48
) {
  list(
    vl_dist = vl_dist(vl_dist_norm),
    hutch_78 = hutch_78,
    melody_dist = melody_dist,
    outer_parallels = outer_parallels,
    any_parallels = any_parallels,
    dist_above_top = dist_above_top(top),
    dist_from_middle = dist_from_middle(middle),
    dist_below_bottom = dist_below_bottom(bottom),
    part_overlap = part_overlap,
    exposed_outer_octaves = exposed_outer_octaves,
    diff_num_notes = diff_num_notes
  )
}
