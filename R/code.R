default_cost_funs <- function() {
  roughness <- cost_fun(context_sensitive = FALSE, f = function(x) {
    HarmonyDissonance::get_roughness(frequency = x,
                                     frequency_scale = "midi",
                                     method = "hutch")
  }, weight = 20)

  vl_dist <- cost_fun(context_sensitive = TRUE, f = function(context, x) {
    vldist::vl_dist(context, x, elt_type = "pitch", norm = "taxicab")
  }, weight = 10)

  as.list(environment())
}
