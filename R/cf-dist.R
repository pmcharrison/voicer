melody_dist <- (function(x, y) {
  abs(max(y) - max(x))
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE)

dist_above_top <- function(top) {
  (function(x) {
    pmax(0, max(x) - top)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

dist_from_middle <- function(middle) {
  (function(x) {
    abs(mean(x) - middle)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

dist_below_bottom <- function(bottom) {
  (function(x) {
    pmax(0, bottom - min(x))
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}

.min_vl_dists <- memoise::memoise(minVL::min_vl_dists,
                                  cache = memoise::cache_memory())

.min_vls <- memoise::memoise(minVL::min_vls,
                             cache = memoise::cache_memory())

vl_dist <- function(vl_dist_norm) {
  (function(contexts, x) {
    .min_vl_dists(contexts,
                  list(x),
                  elt_type = "pitch",
                  norm = vl_dist_norm)
  }) %>%
    seqopt::cost_fun(context_sensitive = TRUE,
                     vectorised = TRUE)

}
