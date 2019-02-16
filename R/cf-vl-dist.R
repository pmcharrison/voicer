..min_vls_cache <- list(store = memoise::cache_memory(),
                        counter = 0L) %>% as.environment()

..min_vls <- memoise::memoise(minVL::min_vls, cache = ..min_vls_cache$store)

.min_vls <- function(...) {
  cache <- ..min_vls_cache
  if (cache$counter > 1000L) memoise::forget(..min_vls)
  cache$counter <- cache$counter + 1L
  ..min_vls(...)
}

#' Voice-leading distance
#' 
#' Computes the minimal voice-leading distance (in semitones)
#' between two chords,
#' using the minimal voice-leading algorithm
#' of \insertCite{Tymoczko2006;textual}{voicer}
#' as implemented in the \code{minVL} package (see \code{\link[minVL]{min_vl}}).
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
#' A numeric vector of the same length as \code{contexts},
#' with the ith element corresponding to the sum voice-leading distance
#' between the ith element of \code{contexts} and \code{continuation}.
#' 
#' @note 
#' Unlike Tymoczko's original presentation, 
#' distances are computed in pitch space, not pitch-class space.
#' 
#' @references 
#' \insertAllCited{}
#' 
#' @export
vl_dist <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  vl_dist_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
