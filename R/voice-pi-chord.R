#' @export
voice.vec_pi_chord <- function(x, opt = voice_opt()) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty chords not permitted")
  if (opt$verbose) message("Enumerating all possible chord voicings...")
  y <- all_voicings_vec_pi_chord(x, opt)
  if (any(purrr::map_lgl(y, function(z) length(z) == 0L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y,
                  cost_funs = opt$cost_funs,
                  weights = opt$weights, 
                  minimise = FALSE,
                  exponentiate = TRUE,
                  verbose = opt$verbose) %>%
    hrep::vec(type = "pi_chord")
}

all_voicings_vec_pi_chord <- function(x, opt) {
  purrr::map(x, all_voicings_pi_chord,
             opt$min_octave, opt$max_octave,
             opt$dbl_change, opt$dbl_min, opt$dbl_max)
}

#' All voicings (pi_chord)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pi_chord}}.
#' @param x Object to voice.
#' @return A list of possible voicings.
#' @export
all_voicings_pi_chord <- function(x,
                                  min_octave, max_octave,
                                  dbl_change, dbl_min, dbl_max) {
  x <- as.numeric(x)
  if (length(x) == 0L) stop("empty chords not permitted")
  x <- sort(x)
  bass_pc <- x[1] %% 12
  x <- if (dbl_change) {
    pc_set <- sort(unique(x %% 12))
    all_voicings_pc_set(pc_set,
                        min_octave, max_octave,
                        dbl_change, dbl_min, dbl_max)
  } else {
    pc_multiset <- sort(x %% 12)
    all_voicings_pc_multiset(pc_multiset, min_octave, max_octave)
  }
  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
