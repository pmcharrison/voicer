#' @param min_octave Integerish scalar;
#' the minimum octave allowed in the voicing,
#' expressed relative to middle C (MIDI note 60).
#' For example, -1 identifies the octave ranging from one octave below middle C
#' to the B below middle C.
#' @param max_octave Integerish scalar;
#' the maximum octave allowed in the voicing,
#' expressed relative to middle C (MIDI note 60).
#' For example, 0 identifies the octave ranging from middle C
#' to the B 11 semitones above.
#' @param dbl_change Is it permitted to change the doubling of the chords,
#' whether by adding duplicated pitch classes or removing duplicated pitch classes?
#' @param dbl_min If \code{dbl_change} is \code{TRUE},
#' the minimum allowed number of notes in the voiced chords.
#' @param dbl_max If \code{dbl_change} is \code{TRUE},
#' the maximum allowed number of notes in the voiced chords.
#' @export
voice_opt <- function(min_octave = -2L,
                      max_octave = 1L,
                      dbl_change = FALSE,
                      dbl_min = 2L,
                      dbl_max = 4L,
                      cost_funs = voice_cost_funs(),
                      weights = voice_weights(),
                      verbose = TRUE) {
  
  weights <- format_weights(weights)
  check_weight_consistency(weights, cost_funs)
  
  stopifnot(min_octave <= max_octave)
  stopifnot(dbl_min <= dbl_max)
  as.list(environment())
}

check_weight_consistency <- function(weights, cost_funs) {
  if (length(weights) != length(cost_funs))
    stop("weights must have the same length as cost_funs")
  
  if (anyNA(weights)) stop("weights are not permitted to be NA")
  
  if (!is.null(names(cost_funs)) && !is.null(names(weights))
      && !identical(names(cost_funs), names(weights)))
    stop("inconsistent names for cost_funs and weights")
}

format_weights <- function(x) {
  UseMethod("format_weights")
}

format_weights.default <- function(x) {
  stop("don't know how to deal with weights of class ",
       paste(class(x), collapse = ", "))
}

format_weights.numeric <- function(x) {
  x
}

format_weights.voicer_weights <- function(x) {
  x$estimate %>% magrittr::set_names(x$feature)
}

format_weights.voicer_model <- function(x) {
  format_weights(x$weights)
}

#' @export
voice_weights <- function(...) {
  arg <- unlist(list(...))
  if (length(arg) > 0L && !is.numeric(arg))
    stop("voice_weights did not receive a numeric input")
  if (anyNA(arg)) stop("NA values not permitted in voice_weights")
  if (anyDuplicated(arg)) stop("duplicates not permitted in voice_weights")
  if (length(arg) > 0L && is.null(names(arg)))
    stop("all arguments to voice_weights must be named")
  funs <- names(voice_cost_funs())
  unrecognised <- names(arg)[!(names(arg) %in% funs)]
  if (length(unrecognised) > 0L)
    stop("unrecognised cost function(s): ", paste(unrecognised, collapse = ", "))
  weights <- rep(1, times = length(funs))
  names(weights) <- funs
  for (i in seq_along(arg)) {
    name <- names(arg)[i]
    value <- arg[i]
    weights[name] <- value
  }
  weights
}
