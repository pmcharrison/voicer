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
#' @param fix_melody
#' A numeric vector specifying the desired melody notes (i.e. top line)
#' for the voicing, as MIDI pitches.
#' The ith element should correspond to the melody note for the ith chord of x.
#' NA values mean that the melody is unconstrained.
#' 
#' @param fix_content
#' A list of potentially empty numeric vectors specifying pitches 
#' that must be included in the corresponding chords of x.
#' Can be used to produce melodies internal to the texture.
#' 
#' @param fix_chords
#' A list of voicings to fix in advance within the generated result,
#' each given as numeric vectors of MIDI pitches.
#' A NULL element means that the voicing for the corresponding chord is unconstrained.
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
voice <- function(x, 
                  opt = voice_opt(), 
                  fix_melody = rep(NA_integer_, times = length(x)), 
                  fix_content = rep(list(), times = length(x)),
                  fix_chords = vector("list", length(x))) {
  if (!(is.numeric(fix_melody) && 
        length(fix_melody) == length(x)))
    stop("fix_melody must either be NULL or a numeric vector of same length as x")
  
  if (!(is.list(fix_content) && 
        length(fix_content) == length(x)))
    stop("fix_content must either be NULL or a list of same length as x")
  
  if (!(is.list(fix_chords) && 
        length(fix_chords) == length(x)))
    stop("fix_chords must either be NULL or a list of same length as x")
  
  UseMethod("voice")
}

#' @export
voice.vec <- function(x, 
                      opt = voice_opt(),
                      fix_melody = NULL, 
                      fix_content = NULL,
                      fix_chords = NULL) {
  if (length(x) == 0L) return(x)
  type <- hrep::type(x)
  checkmate::qassert(type, "S1")
  f <- paste0("voice.vec_", type)
  do.call(f, args = list(x = x, 
                         opt = opt,
                         fix_melody = fix_melody, 
                         fix_content = fix_content,
                         fix_chords = fix_chords))
}

#' @export
voice.coded_vec <- function(x, 
                            opt = voice_opt(),
                            fix_melody = NULL, 
                            fix_content = NULL,
                            fix_chords = NULL) {
  voice(hrep::decode(x), 
        opt = opt,
        fix_melody = fix_melody, 
        fix_content = fix_content,
        fix_chords = fix_chords)
}
