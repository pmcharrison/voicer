#' Voice
#' 
#' Generates an optimized voicing for a chord sequence.
#' 
#' By default, the algorithm optimizes the sum of the linear predictors
#' for each chord transition.
#' By setting the \code{exp_cost}, \code{norm_cost}, and \code{log_cost}
#' arguments of \code{\link{voice_opt}} to \code{TRUE},
#' it is possible alternatively to optimize the probability of the sequence.
#' 
#' @note 
#' Voicing sequences of type \code{\link{pi_chord}} preserves 
#' the original bass pitch classes.
#' The original doublings for each pitch class can also be preserved
#' by setting \code{dbl_change = TRUE} in \code{\link{voice_opt}}.
#' 
#' @param x 
#' Chord sequence to voice.
#' This should take the form of a \code{\link[hrep]{vec}}
#' object from the \code{hrep} package.
#' A \code{\link[hrep]{vec}} object is essentially a typed list;
#' here the type should be one of three chord representations from
#' the \code{hrep} package, namely
#' \code{\link{pc_chord}}, \code{\link{pc_set}}, or \code{\link{pi_chord}}.
#' See \bold{Note} for more information.
#' 
#' @param opt
#' A list of options as created by \code{\link{voice_opt}}.
#' 
#' @return 
#' A voiced chord sequence,
#' represented as a \code{\link[hrep]{vec}} object 
#' with type \code{\link[hrep]{pi_chord}}.
#' This object can be inspected using \code{\link[base]{as.list}}.
#' 
#' @examples
#' library(magrittr)
#' library(hrep)
#' list(
#'   pc_chord(c(0, 4, 7)),
#'   pc_chord(c(0, 5, 9)),
#'   pc_chord(c(2, 7, 11))
#' ) %>%
#'   vec("pc_chord") %>%
#'   voice(voice_opt(min_notes = 3, max_notes = 3))
#'
#' @export
voice <- function(x, opt = voice_opt()) {
  UseMethod("voice")
}

#' @export
voice.vec <- function(x, opt = voice_opt()) {
  if (length(x) == 0L) return(x)
  type <- hrep::type(x)
  checkmate::qassert(type, "S1")
  f <- paste0("voice.vec_", type)
  do.call(f, args = list(x = x, opt = opt))
}

#' @export
voice.coded_vec <- function(x, opt = voice_opt()) {
  voice(hrep::decode(x), opt = opt)
}
