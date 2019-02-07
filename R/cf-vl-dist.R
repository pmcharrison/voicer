.min_vls <- memoise::memoise(minVL::min_vls,
                             cache = memoise::cache_memory())

vl_dist <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  vl_dist_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE)
