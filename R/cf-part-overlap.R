#' @export
part_overlap <- (function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(FALSE)
  vl <- min_vl(x, y)
  n <- length(vl$start)
  cross_above <- any(vl$end[- n] > vl$start[- 1L])
  cross_below <- any(vl$end[- 1L] < vl$start[- n])
  cross_above || cross_below
}) %>% seqopt::cost_fun(context_sensitive = TRUE)
