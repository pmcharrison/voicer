#' Profile features
#' 
#' Computes profiling information for a set of features.
#' These features are computed for a transition between two chords.
#' 
#' @param random
#' (Logical scalar)
#' Whether the two chords should be resampled each time or not.
#' The latter is useful for evaluating the effect of memoization on function speeds.
#' 
#' @param features
#' A named list of feature functions, defaulting to the list created by 
#' \code{\link{voice_features}}.
#' 
#' @return
#' A named numeric vector of computation times for each feature,
#' with units of seconds.
#' 
#' @export
profile_features <- function(random = TRUE, features = voice_features()) {
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
                           min_notes = 3,
                           max_notes = 4)
  y <- all_voicings_pc_set(b,
                           min_octave = - 2,
                           max_octave = 1,
                           dbl_change = TRUE,
                           min_notes = 3,
                           max_notes = 4)[[1]]

  seqopt::cost_by_prev_state(x, y,
                             cost_funs = features,
                             weights = rep(1, times = length(features)),
                             exp_cost = FALSE,
                             profile = TRUE) %>%
    attr("time")
}
