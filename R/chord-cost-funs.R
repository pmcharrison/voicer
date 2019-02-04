#' @export
voicer_cost_funs <- function(
  memoise = TRUE,
  roughness_weight = 1,
  roughness_method = "hutch",
  parallels_weight = 1,
  melody_dist_weight = 1,
  vl_dist_weight = 1,
  vl_dist_norm = "taxicab",
  mean_pitch = 60,
  mean_pitch_weight = 1,
  max_pitch_boundary = 80,
  max_pitch_weight = 1,
  min_pitch_boundary = 40,
  min_pitch_weight = 1
) {
  list(
    roughness = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      incon::incon(x, model = "hutch_78_roughness")
    }, memoise = TRUE, weight = roughness_weight),

    parallels = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
      as.numeric(parallels(context, x))
    }, weight = parallels_weight),

    melody_dist = seqopt::cost_fun(context_sensitive = TRUE,
                                   f = melody_dist,
                                   weight = melody_dist_weight),

    min_vl_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(contexts, x) {
      as.numeric(minVL::min_vl_dists(contexts, list(x), elt_type = "pitch", norm = vl_dist_norm))
    }, memoise = TRUE, vectorised = TRUE, weight = vl_dist_weight),

    mean_pitch = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      abs(mean(x) - mean_pitch)
    }, weight = mean_pitch_weight),

    max_pitch = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      pmax(0, max(x) - max_pitch_boundary)
    }, weight = max_pitch_weight),

    min_pitch = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      pmax(0, min_pitch_boundary - min(x))
    }, weight = min_pitch_weight)

  )
}

# Checks for parallels between the bass and melody ONLY
#' @export
parallels <- function(x, y) {
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
