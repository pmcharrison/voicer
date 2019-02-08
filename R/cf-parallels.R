#' @export
any_parallels <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  any_parallels_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)

# Checks for parallels between the bass and melody ONLY
#' @export
outer_parallels <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  outer_parallels_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
