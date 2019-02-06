#' @export
part_overlap <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(FALSE)
  vl <- min_vl(x, y)
  n <- length(vl$start)
  cross_above <- any(vl$end[- n] > vl$start[- 1L])
  cross_below <- any(vl$end[- 1L] < vl$start[- n])
  cross_above || cross_below
}

cf_part_overlap <- function() {
  seqopt::cost_fun(context_sensitive = TRUE,
                   f = function(context, x) as.numeric(part_overlap(context, x)))
}
