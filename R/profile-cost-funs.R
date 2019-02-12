#' @export
profile_cost_funs <- function(random = TRUE, funs = voice_cost_funs()) {
  if (random) {
    a <- sort(sample(0:11, size = 3))
    b <- sort(sample(0:11, size = 3))
  } else {
    a <- c(0, 4, 7)
    b <- c(2, 7, 11)
  }

  x <- all_voicings_pc_set(a,
                           min_octave = - 2,
                           max_octave = 1,
                           dbl_change = TRUE,
                           dbl_min = 3,
                           dbl_max = 4)
  y <- all_voicings_pc_set(b,
                           min_octave = - 2,
                           max_octave = 1,
                           dbl_change = TRUE,
                           dbl_min = 3,
                           dbl_max = 4)[[1]]

  seqopt::cost_by_prev_state(x, y,
                             cost_funs = funs,
                             weights = rep(1, times = length(funs)),
                             exp_cost = FALSE,
                             profile = TRUE) %>%
    attr("time")
}
