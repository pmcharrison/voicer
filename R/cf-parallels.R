#' @export
any_parallels <- (function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(FALSE)
  min_vl <- min_vl(x, y, elt_type = "pitch")
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
}) %>% seqopt::cost_fun(context_sensitive = TRUE)

# Checks for parallels between the bass and melody ONLY
#' @export
outer_parallels <- (function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(FALSE)
  x_bass <- min(x)
  x_treble <- max(x)
  x_int <- x_treble - x_bass
  if ((x_int %% 12) %in% c(0, 7)) {
    y_bass <- min(y)
    y_treble <- max(y)
    y_int <- y_treble - y_bass
    if (y_int == x_int && x_bass != y_bass) TRUE else FALSE
  } else FALSE
}) %>% seqopt::cost_fun(context_sensitive = TRUE)
