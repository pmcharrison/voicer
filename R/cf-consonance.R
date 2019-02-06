hutch_78 <- function(x) incon::incon(x, model = "hutch_78_roughness")
hutch_78 <- memoise::memoise(hutch_78, cache = memoise::cache_memory())

cf_hutch_78 <- function() {
  seqopt::cost_fun(context_sensitive = FALSE, f = hutch_78)
}
