#' @export
melody_dist <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  melody_dist_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE)

#' @export
dist_above_top <- function(top) {
  (function(x) {
    pmax(0, max(x) - top)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

#' @export
dist_from_middle <- function(middle) {
  (function(x) {
    abs(mean(x) - middle)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

#' @export
dist_below_bottom <- function(bottom) {
  (function(x) {
    pmax(0, bottom - min(x))
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}
