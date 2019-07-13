#' @export
voice.vec_pi_chord <- function(x, 
                               opt = voice_opt(),
                               fix_melody = NULL, 
                               fix_content = NULL,
                               fix_chords = NULL) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty chords not permitted")
  if (opt$verbose) message("Enumerating all possible chord voicings...")
  y <- all_voicings_vec_pi_chord(x, opt)
  if (any(purrr::map_lgl(y, function(z) length(z) == 0L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y,
                  cost_funs = opt$features,
                  weights = opt$weights, 
                  minimise = FALSE,
                  exp_cost = opt$exp_cost,
                  norm_cost = opt$norm_cost,
                  log_cost = opt$log_cost,
                  verbose = opt$verbose) %>%
    hrep::vec(type = "pi_chord")
}

all_voicings_vec_pi_chord <- function(x, opt) {
  purrr::map(x, all_voicings_pi_chord,
             opt$min_octave, opt$max_octave,
             opt$dbl_change, opt$min_notes, opt$max_notes)
}

#' All voicings (pi_chord)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pi_chord}}.
#' 
#' By default, these voicings preserve the chord's bass pitch class.
#' To include all possible inversions,
#' transform the chord first to an object of class
#' \code{\link[hrep]{pc_set}} and then call
#' \code{\link{all_voicings_pc_set}}.
#' By setting \code{dbl_change} to \code{TRUE},
#' it is possible to preserve the pitch-class doublings
#' in the original chord.
#' 
#' @param x Object to voice.
#' @param min_octave See \code{\link{voice_opt}}.
#' @param max_octave See \code{\link{voice_opt}}.
#' @param dbl_change See \code{\link{voice_opt}}.
#' @param min_notes See \code{\link{voice_opt}}.
#' @param max_notes See \code{\link{voice_opt}}.
#' 
#' @return A list of possible voicings.
#' @export
all_voicings_pi_chord <- function(x,
                                  min_octave, max_octave,
                                  dbl_change, min_notes, max_notes) {
  x <- as.numeric(x)
  if (length(x) == 0L) stop("empty chords not permitted")
  x <- sort(x)
  bass_pc <- x[1] %% 12
  x <- if (dbl_change) {
    pc_set <- sort(unique(x %% 12))
    all_voicings_pc_set(pc_set,
                        min_octave, max_octave,
                        dbl_change, min_notes, max_notes)
  } else {
    pc_multiset <- sort(x %% 12)
    all_voicings_pc_multiset(pc_multiset, min_octave, max_octave)
  }
  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
