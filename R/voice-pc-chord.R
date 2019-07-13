voice.vec_pc_chord <- function(x, 
                               opt = voice_opt(),
                               fix_melody = NULL, 
                               fix_content = NULL,
                               fix_chords = NULL) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty chords not permitted")
  if (opt$verbose) message("Enumerating all possible chord voicings...")
  y <- all_voicings_vec_pc_chord(x = x, opt = opt)
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

all_voicings_vec_pc_chord <- function(x, opt) {
  purrr::map(x, all_voicings_pc_chord,
             opt$min_octave, opt$max_octave,
             opt$dbl_change, opt$min_notes, opt$max_notes)
}


#' All voicings (pc_chord)
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pc_chord}}.
#' @param x Object to voice.
#' @param min_octave See \code{\link{voice_opt}}.
#' @param max_octave See \code{\link{voice_opt}}.
#' @param dbl_change See \code{\link{voice_opt}}.
#' @param min_notes See \code{\link{voice_opt}}.
#' @param max_notes See \code{\link{voice_opt}}.
#' @return A list of possible voicings.
#' @export
all_voicings_pc_chord <- function(x,
                                  min_octave, max_octave,
                                  dbl_change, min_notes, max_notes) {
  x <- as.numeric(x)
  if (length(x) == 0L) stop("empty chords not permitted")
  bass_pc <- hrep::get_bass_pc(x)
  all_pc <- as.numeric(x)

  x <- if (dbl_change)
    all_voicings_pc_set(all_pc,
                        min_octave, max_octave,
                        dbl_change, min_notes, max_notes) else
      all_voicings_pc_multiset(all_pc, min_octave, max_octave)

  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
all_voicings_pc_chord <- memoise::memoise(all_voicings_pc_chord)
