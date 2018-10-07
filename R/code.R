voice_chord_cost <- function(
  roughness_weight = 20,
  roughness_method = "hutch",
  melody_dist_weight = 10,
  vl_dist_weight = 10,
  vl_dist_norm = "taxicab"
  mean_pitch = 60,
  mean_pitch_weight = 10,
  max_pitch_boundary = 80,
  max_pitch_weight = 5,
  min_pitch_boundary = 40,
  min_pitch_weight = 5
) {
  list(
  roughness = seqopt::cost_fun(context_sensitive = FALSE, f = function(x) {
    HarmonyDissonance::get_roughness(frequency = x,
                                     frequency_scale = "midi",
                                     method = "hutch")
  }, weight = roughness_weight),

  melody_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
    abs(max(x) - max(context))
  }, weight = melody_dist_weight),

  vl_dist = seqopt::cost_fun(context_sensitive = TRUE, f = function(context, x) {
    vldist::vl_dist(context, x, elt_type = "pitch", norm = vl_dist_norm)
  }, weight = 10)

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
