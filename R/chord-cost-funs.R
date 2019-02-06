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

#' @export
voicer_cost_funs <- function(
  weights = voicer_weights(), # leave at default to replicate Vincent's stimuli
  memoise = TRUE,
  vl_dist_norm = "taxicab",
  top = 72, # set to 80 to replicate Vincent's stimuli
  middle = 60, # set to 60 to replicate Vincent's stimuli
  bottom = 48, # set to 40 to replicate Vincent's stimuli
  extra = list()
) {
  stopifnot(is.list(extra))
  if (!all(purrr::map_lgl(extra, seqopt::is.cost_fun)))
    stop("not all elements of 'extra' were cost functions")

  default <- list(
    vl_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(contexts, x) {
      as.numeric(minVL::min_vl_dists(contexts, list(x), elt_type = "pitch", norm = vl_dist_norm))
    }, memoise = TRUE, vectorised = TRUE, weight = weights[["vl_dist"]]),

    hutch_78 = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      incon::incon(x, model = "hutch_78_roughness")
    }, memoise = TRUE, weight = weights[["hutch_78"]]),

    melody_dist = seqopt::cost_fun(context_sensitive = TRUE,
                                   f = melody_dist,
                                   weight = weights[["melody_dist"]]),

    outer_parallels = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
      as.numeric(outer_parallels(context, x))
    }, weight = weights[["outer_parallels"]]),

    dist_from_middle = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      abs(mean(x) - middle)
    }, weight = weights[["dist_from_middle"]]),

    dist_above_top = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      pmax(0, max(x) - top)
    }, weight = weights[["dist_above_top"]]),

    dist_below_bottom = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      pmax(0, bottom - min(x))
    }, weight = weights[["dist_below_bottom"]])
  )
  c(default, extra)
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
