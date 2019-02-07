#' @export
any_parallels <- any_parallels_ %>% seqopt::cost_fun(context_sensitive = TRUE,
                                                     vectorised = TRUE)

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
