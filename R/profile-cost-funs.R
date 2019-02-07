#' @export
profile_cost_funs <- function(funs = voice_cost_funs()) {
  x <- all_voicings_pc_set(c(0, 4, 7),
                           min_octave = - 2,
                           max_octave = 1,
                           dbl_change = TRUE,
                           dbl_min = 3,
                           dbl_max = 4)
  y <- all_voicings_pc_set(c(2, 7, 11),
                           min_octave = - 2,
                           max_octave = 1,
                           dbl_change = TRUE,
                           dbl_min = 3,
                           dbl_max = 4)[[1]]

  seqopt::cost_by_prev_state(x, y,
                             cost_funs = funs,
                             weights = rep(1, times = length(funs)),
                             exponentiate = FALSE,
                             profile = TRUE) %>%
    attr("time")
}
