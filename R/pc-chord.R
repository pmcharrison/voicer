voice.vec_pc_chord <- function(x, opt = voice_opt()) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty chords not permitted")
  y <- all_voicings_vec_pc_chord(x = x, opt = opt)
  if (any(purrr::map_lgl(y, function(z) length(z) == 0L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y, cost_funs = opt$cost_funs, progress = opt$progress) %>%
    hrep::vec(type = "pi_chord")
}

all_voicings_vec_pc_chord <- function(x, opt) {
  purrr::map(x, all_voicings_pc_chord,
             opt$min_octave, opt$max_octave,
             opt$dbl_change, opt$dbl_min, opt$dbl_max)
}


#' All voicings (pc_chord)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pc_chord}}.
#' @param x Object to voice.
#' @return A list of possible voicings.
#' @export
all_voicings_pc_chord <- function(x,
                                  min_octave, max_octave,
                                  dbl_change, dbl_min, dbl_max) {
  if (length(x) == 0L) stop("empty chords not permitted")
  bass_pc <- hrep::get_bass_pc(x)
  all_pc <- as.numeric(x)

  x <- if (dbl_change)
    all_voicings_pc_set(all_pc,
                        min_octave, max_octave,
                        dbl_change, dbl_min, dbl_max) else
      all_voicings_pc_multiset(all_pc, min_octave, max_octave)

  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
