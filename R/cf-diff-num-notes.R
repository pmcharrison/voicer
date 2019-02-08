diff_num_notes <- function(ideal_num_notes) {
  checkmate::qassert(ideal_num_notes, "N1[0,]")
  (function(x) {
    abs(length(x) - ideal_num_notes)
  }) %>%
    seqopt::cost_fun(context_sensitive = FALSE)
}
