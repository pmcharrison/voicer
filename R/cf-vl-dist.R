..min_vls_cache <- list(store = memoise::cache_memory(),
                        counter = 0L) %>% as.environment()

..min_vls <- memoise::memoise(minVL::min_vls, cache = ..min_vls_cache$store)

.min_vls <- function(...) {
  cache <- ..min_vls_cache
  if (cache$counter > 1000L) memoise::forget(..min_vls)
  cache$counter <- cache$counter + 1L
  ..min_vls(...)
}

vl_dist <- (function(contexts, continuation) {
  stopifnot(is.list(contexts), is.numeric(continuation))
  vl_dist_(contexts, continuation)
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE,
                   vectorised = TRUE,
                   symmetric = TRUE)
