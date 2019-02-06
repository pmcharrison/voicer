#' @export
diff_num_notes <- (function(x, y) {
  abs(length(x) - length(y))
}) %>%
  seqopt::cost_fun(context_sensitive = TRUE)
