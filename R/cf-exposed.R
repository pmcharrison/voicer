#' @export
exposed_outer_octaves <- (function(x, y) {
  y_bass <- min(y)
  y_treb <- max(y)
  y_int <- y_treb - y_bass
  y_outer_octave <- (y_int > 0) && (y_int %% 12 == 0)
  res <- if (y_outer_octave) {
    x_bass <- min(x)
    x_treb <- max(x)
    treb_motion <- y_treb - x_treb
    bass_motion <- y_bass - x_bass
    similar_motion <- (treb_motion * bass_motion) > 0
    similar_motion &&
      abs(treb_motion) > 2L &&
      abs(bass_motion) > 2L
  } else {
    FALSE
  }
}) %>% seqopt::cost_fun(context_sensitive = TRUE)
