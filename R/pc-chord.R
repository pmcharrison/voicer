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
  purrr::map(x, function(y) all_voicings_pc_chord(y, opt))
}

all_voicings_pc_chord <- function(x, opt) {
  if (length(x) == 0L) stop("empty chords not permitted")
  bass_pc <- hrep::get_bass_pc(x)
  all_pc <- as.numeric(x)

  x <- if (opt$dbl_change)
    all_voicings_pc_set(x = all_pc, opt) else
      all_voicings_pc_multiset(x = all_pc, opt)

  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
