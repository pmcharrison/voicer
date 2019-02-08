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
