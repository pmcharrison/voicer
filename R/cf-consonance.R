cf_hutch_78 <- function(memoise) {
  seqopt::cost_fun(
    context_sensitive = FALSE,
    memoise = memoise,
    f = function(x) incon::incon(x, model = "hutch_78_roughness"))
}
