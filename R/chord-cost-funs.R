#' @export
voicer_cost_funs <- function(
  memoise = TRUE,
  vl_dist_norm = "taxicab",
  top = 72, # set to 80 to replicate Vincent's stimuli
  middle = 60, # set to 60 to replicate Vincent's stimuli
  bottom = 48 # set to 40 to replicate Vincent's stimuli
) {
  list(
    vl_dist = cf_vl_dist(memoise, vl_dist_norm),
    hutch_78 = cf_hutch_78(memoise),
    melody_dist = cf_melody_dist(),
    outer_parallels = cf_outer_parallels(),
    dist_above_top = cf_dist_above_top(top),
    dist_from_middle = cf_dist_from_middle(middle),
    dist_below_bottom = cf_dist_below_bottom(bottom)
  )
}

cf_vl_dist <- function(memoise, vl_dist_norm) {
  seqopt::cost_fun(
    context_sensitive = TRUE,
    memoise = memoise,
    vectorised = TRUE,
    f = function(contexts, x)
      as.numeric(minVL::min_vl_dists(contexts,
                                     list(x),
                                     elt_type = "pitch",
                                     norm = vl_dist_norm)))
}

cf_hutch_78 <- function(memoise) {
  seqopt::cost_fun(
    context_sensitive = FALSE,
    memoise = memoise,
    f = function(x) incon::incon(x, model = "hutch_78_roughness"))
}

cf_melody_dist <- function() {
  seqopt::cost_fun(context_sensitive = TRUE, f = melody_dist)
}

cf_outer_parallels <- function() {
  seqopt::cost_fun(
    context_sensitive = TRUE,
    f = function(context, x) as.numeric(outer_parallels(context, x)))
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

# Checks for parallels between the bass and melody ONLY
#' @export
outer_parallels <- function(x, y) {
  x_bass <- min(x)
  x_treble <- max(x)
  x_int <- x_treble - x_bass
  if ((x_int %% 12) %in% c(0, 7)) {
    y_bass <- min(y)
    y_treble <- max(y)
    y_int <- y_treble - y_bass
    if (y_int == x_int && x_bass != y_bass) TRUE else FALSE
  } else FALSE
}

#' @export
melody_dist <- function(x, y) {
  abs(max(y) - max(x))
}

#' @export
any_parallels <- function(x, y) {
  min_vl <- minVL::min_vl(x, y, elt_type = "pitch")
  n <- length(min_vl$start)
  pairs <- gtools::combinations(n, 2)
  for (i in seq_len(nrow(pairs))) {
    v1 <- pairs[i, 1]
    v2 <- pairs[i, 2]
    moved <- (min_vl$end[v1] - min_vl$start[v1]) != 0
    if (moved) { # if the lower note has moved
      fifth_or_octave <- ((min_vl$start[v2] - min_vl$start[v1]) %% 12) %in% c(0, 7)
      if (fifth_or_octave) { # if the two notes originally formed a (compound) fifth or octave
        parallel <- # if the two notes moved by the same interval
          (min_vl$end[v1] - min_vl$start[v1]) ==
          (min_vl$end[v2] - min_vl$start[v2])
        if (parallel) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

# part overlap rule?

# exposed octaves and fifths

# changes in the number of chord notes

#' @export
voicer_weights <- function(hutch_78 = 1,
                           vl_dist = 1,
                           melody_dist = 1,
                           outer_parallels = 1,
                           dist_from_middle = 1,
                           dist_above_top = 1,
                           dist_below_bottom = 1) {
  res <- unlist(as.list(environment()))
  stopifnot(is.numeric(res))
  res
}
