#' Any parallels
#' 
#' Checks for parallel octaves or fifths 
#' within a chord transition.
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
#' \code{TRUE} if parallel octaves or fifths are found
#' between the ith element of \code{contexts} and 
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
any_parallels <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  any_parallels_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)

#' Outer parallels
#' 
#' Equivalent to \code{\link{any_parallels}},
#' but only returns \code{TRUE} when the parallel octaves
#' are found between the bass and melody lines.
#' 
#' @param contexts See \code{\link{any_parallels}}.
#' @param continuation See \code{\link{any_parallels}}.
#' @export
outer_parallels <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  outer_parallels_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
