#' Part overlap
#' 
#' Checks for part overlap within a chord transition.
#' Part overlap occurs when the pitch height of a certain voice
#' exceeds the pitch height of a higher voice from the previous chord,
#' or alternatively when the pitch height of a certain voice
#' is less than the pitch height of a lower voice from the previous chord.
#' 
#' Voice parts are inferred using the minimal voice-leading distance algorithm
#' of \insertCite{Tymoczko2006;textual}{voicer}
#' as implemented in the \code{minVL} package (see \code{\link[minVL]{min_vl}}).
#' 
#' @param contexts
#' A list of chords corresponding to the first chord
#' in the transition. 
#' Each chord should be represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' \code{any_parallels} is vectorized over this argument.
#' 
#' @param continuation
#' The second chord in the transition,
#' represented as a numeric vector
#' of MIDI note numbers, without duplicates,
#' sorted in ascending order.
#' 
#' @return 
#' A logical vector of the same length as \code{contexts},
#' with the ith element being 
#' \code{TRUE} if part overlap is found 
#' when the ith element of \code{contexts} moves to 
#' \code{continuation}, and \code{FALSE} otherwise.
#' 
#' @note 
#' This is a \code{voicer} feature suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}}.
#' 
#' @references 
#' \insertAllCited{}
#' 
#' @export
part_overlap <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  part_overlap_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
