cheung_2019_opt <- function(verbose = TRUE) {
  features <- voice_features(top = 80, middle = 60, bottom = 40)[c(
    "dist_above_top", "dist_below_bottom", "dist_from_middle",
    "hutch_78", "melody_dist", "outer_parallels", "vl_dist"
  )]
  weights <- rep(-1, times = length(features))
  voice_opt(features = features, 
            weights = weights,
            exp_cost = FALSE,
            min_octave = -2L,
            max_octave = 1L,
            dbl_change = FALSE,
            min_notes = 2L,
            max_notes = 4L,
            verbose = verbose)
}
