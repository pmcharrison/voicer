cf_melody_dist <- function() {
  seqopt::cost_fun(context_sensitive = TRUE, f = melody_dist)
}

cf_dist_above_top <- function(top) {
  seqopt::cost_fun(
    context_sensitive = FALSE,
    f = function(x) pmax(0, max(x) - top))
}

cf_dist_from_middle <- function(middle) {
  seqopt::cost_fun(
    context_sensitive = FALSE,
    f = function(x) abs(mean(x) - middle))
}

cf_dist_below_bottom <- function(bottom) {
  seqopt::cost_fun(
    context_sensitive = FALSE,
    f = function(x) pmax(0, bottom - min(x)))
}

cf_vl_dist <- function(vl_dist_norm) {
  seqopt::cost_fun(
    context_sensitive = TRUE,
    memoise = TRUE,
    vectorised = TRUE,
    f = function(contexts, x)
      as.numeric(minVL::min_vl_dists(contexts,
                                     list(x),
                                     elt_type = "pitch",
                                     norm = vl_dist_norm)))
}

#' @export
melody_dist <- function(x, y) {
  abs(max(y) - max(x))
}
