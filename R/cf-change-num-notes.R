#' @export
change_num_notes <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  change_num_notes_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE)
