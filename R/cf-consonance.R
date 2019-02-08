.hutch_78 <- function(x) incon::incon(x, model = "hutch_78_roughness")
.hutch_78 <- memoise::memoise(.hutch_78)

hutch_78 <- .hutch_78 %>% seqopt::cost_fun(context_sensitive = FALSE)


