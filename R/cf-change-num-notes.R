#' Change in number of notes
#' 
#' Checks for changes in the number of notes 
#' within a chord transition.
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
#' providing the absolute change in the number of chord notes
#' between the ith element of \code{contexts}
#' and \code{continuation}.
#' 
#' @note 
#' This is a \code{voicer} feature suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}}.
#' 
#' @export
change_num_notes <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  change_num_notes_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
