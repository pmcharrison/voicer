#' @export
part_overlap <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  part_overlap_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
