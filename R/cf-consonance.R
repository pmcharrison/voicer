.hutch_78 <- function(x) incon::incon(x, model = "hutch_78_roughness")
.hutch_78 <- memoise::memoise(.hutch_78)


#' Dissonance
#' 
#' Estimates the dissonance of a chord voicing using 
#' Hutchinson and Knopoff's (1978) algorithm as imported 
#' from the \code{\link{incon}} package.
#' The function is memoized to make repeated calls more efficient.
#' 
#' @param x 
#' The chord to analyze,
#' expressed as a numeric vector of MIDI note numbers
#' in ascending order.
#' 
#' @return 
#' A numeric scalar dissonance estimate.
#' 
#' @note 
#' This is a \code{voicer} feature suitable for passing
#' to the \code{features} argument of \code{\link{voice_opt}}.
#' 
#' @export
hutch_78 <- .hutch_78 %>% seqopt::cost_fun(context_sensitive = FALSE)


