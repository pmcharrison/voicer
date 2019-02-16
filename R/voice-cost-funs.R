#' Features
#' 
#' Defines a default set of features for use by 
#' \code{\link{voice}} and \code{\link{get_corpus_features}}.
#' 
#' @param top
#' (Numeric scalar)
#' Passed to \code{\link{dist_above_top}}.
#' 
#' @param middle
#' (Numeric scalar)
#' Passed to \code{\link{dist_from_middle}}.
#' 
#' @param bottom
#' (Numeric scalar)
#' Passed to \code{\link{dist_below_bottom}}.
#' 
#' @param ideal_num_notes
#' Passed to \code{\link{diff_num_notes}}.
#' 
#' @return 
#' Returns a named list of features suitable for passing to 
#' \code{\link{voice_opt}} or \code{\link{get_corpus_features}}.
#' 
#' @export
voice_features <- function(
  top = 72,
  middle = 60,
  bottom = 48,
  ideal_num_notes = 4L
) {
  list(
    any_parallels = any_parallels,
    change_num_notes = change_num_notes,
    diff_num_notes = diff_num_notes(ideal_num_notes),
    dist_above_top = dist_above_top(top),
    dist_below_bottom = dist_below_bottom(bottom),
    dist_from_middle = dist_from_middle(middle),
    exposed_outer_octaves = exposed_outer_octaves,
    hutch_78 = hutch_78,
    melody_dist = melody_dist,
    outer_parallels = outer_parallels,
    part_overlap = part_overlap,
    vl_dist = vl_dist
  )
}
