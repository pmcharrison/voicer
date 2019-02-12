cheung_2019_opt <- function(verbose = TRUE) {
  cost_funs <- voice_cost_funs(top = 80, middle = 60, bottom = 40)[c(
    "dist_above_top", "dist_below_bottom", "dist_from_middle",
    "hutch_78", "melody_dist", "outer_parallels", "vl_dist"
  )]
  weights <- rep(-1, times = length(cost_funs))
  voice_opt(cost_funs = cost_funs, 
            weights = weights,
            exp_cost = FALSE,
            min_octave = -2L,
            max_octave = 1L,
            dbl_change = FALSE,
            dbl_min = 2L,
            dbl_max = 4L,
            verbose = verbose)
}
