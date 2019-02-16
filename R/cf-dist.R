#' Melody distance
#' 
#' Computes the distance traversed by the top voice
#' in a chord transition.
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
#' @return 
#' An integer vector, vectorized over \code{contexts},
#' providing the unsigned distance traversed 
#' by the highest voice in the transition
#' between the ith element of \code{contexts}
#' and \code{continuation}.
#' 
#' @note 
#' This is a \code{voicer} feature suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}}.
#' @export
melody_dist <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  melody_dist_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)

#' Distance above top
#' 
#' Defines and returns a \code{voicer} feature, suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}},
#' that computes the extent to which a chord spans 
#' above a predefined pitch height specified in the 
#' \code{top} argument, in semitones.
#' If the chord does not span above \code{top},
#' then the function returns \code{0}.
#' The resulting function takes one argument, \code{x},
#' corresponding to a chord represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' 
#' @param top
#' (Numeric scalar)
#' The pitch height to which the chord is compared,
#' specified as a MIDI note number where 60 corresponds to middle C.
#' 
#' @note 
#' The resulting function is not vectorized.
#' 
#' @export
dist_above_top <- function(top) {
  (function(x) {
    pmax(0, max(x) - top)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

#' Distance from middle
#' 
#' Defines and returns a \code{voicer} feature, suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}},
#' that compares the chord's mean pitch to a reference pitch
#' specified in the \code{middle} argument.
#' The resulting function takes one argument, \code{x},
#' corresponding to a chord represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order,
#' and returns an unsigned distance in semitones.
#' 
#' @param middle
#' (Numeric scalar)
#' The pitch height to which the chord is compared,
#' specified as a MIDI note number where 60 corresponds to middle C.
#' 
#' @note 
#' The resulting function is not vectorized.
#' 
#' @export
dist_from_middle <- function(middle) {
  (function(x) {
    abs(mean(x) - middle)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

#' Distance below bottom
#' 
#' Defines and returns a \code{voicer} feature, suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}},
#' that computes the extent to which a chord spans 
#' below a predefined pitch height specified in the 
#' \code{bottom} argument, in semitones.
#' If the chord does not span below \code{bottom},
#' then the function returns \code{0}.
#' The resulting function takes one argument, \code{x},
#' corresponding to a chord represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' 
#' @param bottom
#' (Numeric scalar)
#' The pitch height to which the chord is compared,
#' specified as a MIDI note number where 60 corresponds to middle C.
#' 
#' @note 
#' The resulting function is not vectorized.
#' 
#' @export
dist_below_bottom <- function(bottom) {
  (function(x) {
    pmax(0, bottom - min(x))
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}
