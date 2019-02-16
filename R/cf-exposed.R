#' Exposed outer octaves
#' 
#' Checks for exposed octaves within the outer voices of a chord transition.
#' 
#' Exposed octaves, as defined here,
#' occur when the chord transition reaches a chord 
#' with a (potentially compound) octave between the 
#' bass and treble voices
#' that is approached by parallel motion and with both voices
#' moving by more than two semitones.
#' 
#' @param contexts
#' A list of chords corresponding to the first chord
#' in the transition. 
#' Each chord should be represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' The function is vectorized over this argument.
#' 
#' @param continuation
#' The second chord in the transition,
#' represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' 
#' @param reverse
#' (Logical scalar)
#' Whether to reverse the computation so that
#' \code{contexts} instead corresponds to a list of continuations
#' and \code{continuation} corresponds to a single context.
#' 
#' @return 
#' A logical vector of the same length as \code{contexts},
#' with the ith element being 
#' \code{TRUE} if exposed octaves are found
#' between the ith element of \code{contexts} and 
#' \code{continuation}, and \code{FALSE} otherwise.
#' 
#' @note 
#' This is a \code{voicer} feature suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}}.
#' 
#' @export
exposed_outer_octaves <- (function(contexts, continuation, reverse = FALSE) {
  stopifnot(is.list(contexts), is.numeric(continuation),
            checkmate::qtest(reverse, "B1"))
  exposed_outer_octaves_(contexts, continuation, reverse)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = FALSE,
                   has_reverse = TRUE)
