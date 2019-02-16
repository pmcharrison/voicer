#' Difference in the number of notes
#' 
#' Defines and returns a \code{voicer} feature, suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}},
#' that computes how much the number of notes in a chord voicing
#' differs from an ideal number of notes as specified by the
#' \code{ideal_num_notes} argument.
#' The resulting function takes one argument, \code{x},
#' corresponding to a chord represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' 
#' @param ideal_num_notes
#' (Numeric scalar)
#' The ideal number of notes in the chord.
#' 
#' @return 
#' A numeric scalar corresponding to the difference between
#' the number of notes in the chord and
#' an ideal number of notes as determined by the
#' \code{ideal_num_notes} argument.
#' 
#' @note 
#' The resulting function is not vectorized.
#' 
#' @export
diff_num_notes <- function(ideal_num_notes) {
  checkmate::qassert(ideal_num_notes, "N1[0,]")
  (function(x) {
    abs(length(x) - ideal_num_notes)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}
