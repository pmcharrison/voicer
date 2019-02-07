#' @export
exposed_outer_octaves <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  exposed_outer_octaves_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE)
