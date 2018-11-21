#' @export
voicer_cost_funs <- function(
  memoise = TRUE,
  roughness_weight = 20,
  roughness_method = "hutch",
  parallels_weight = 50,
  melody_dist_weight = 10,
  vl_dist_weight = 10,
  vl_dist_norm = "taxicab",
  mean_pitch = 60,
  mean_pitch_weight = 10,
  max_pitch_boundary = 80,
  max_pitch_weight = 5,
  min_pitch_boundary = 40,
  min_pitch_weight = 5
) {
  list(
    roughness = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
      incon::get_roughness(frequency = x,
                           frequency_scale = "midi",
                           method = "hutch")
    }, memoise = TRUE, weight = roughness_weight),

    parallels = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
      context_bass <- min(context)
      context_treble <- max(context)
      context_int <- context_treble - context_bass
      if ((context_int %% 12) %in% c(0, 7)) {
        next_bass <- min(x)
        next_treble <- max(x)
        next_int <- next_treble - next_bass
        if (next_int == context_int) 1 else 0
      } else 0
    }, weight = parallels_weight),

    melody_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
      abs(max(x) - max(context))
    }, weight = melody_dist_weight),

    min_vl_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(contexts, x) {
      as.numeric(minVL::min_vl_dists(contexts, list(x), elt_type = "pitch", norm = vl_dist_norm))
    }, memoise = TRUE, vectorised = TRUE, weight = 10),

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
