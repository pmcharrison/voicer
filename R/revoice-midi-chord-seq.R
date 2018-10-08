revoice_midi_chord_seq <- function(x,
                                   min_octave = -1L,
                                   max_octave = 0L,
                                   cost_funs = chord_cost_funs(),
                                   progress = TRUE) {
  y <- all_midi_chord_seq_revoicings(x = x,
                                     min_octave = min_octave,
                                     max_octave = max_octave)
  if (any(purrr::map_lgl(y, function(z) length(z) == 1L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y, cost_funs = cost_funs, progress = progress)
}

all_midi_chord_seq_revoicings <- function(x, min_octave, max_octave) {
  purrr::map(x, function(y) all_midi_chord_revoicings(y,
                                                      min_octave = min_octave,
                                                      max_octave = max_octave))
}

# Preserves the pitch class of the bass note
# Preserves duplicated pitch classes
#' @export
all_midi_chord_revoicings <- function(x, min_octave, max_octave) {
  if (length(x) == 0L) return(list())
  x <- sort(x)
  pcs <- x %% 12
  bass_pc <- pcs[1]
  octaves <- seq(from = min_octave, to = max_octave)
  gtools::permutations(n = length(octaves),
                            r = length(pcs),
                            v = 60L + 12L * octaves,
                            repeats.allowed = TRUE) %>%
    sweep(MARGIN = 2, STATS = pcs, FUN = "+") %>%
    (function(y) purrr::map(seq_len(nrow(y)), function(i) sort(y[i, ]))) %>%
    purrr::keep(.p = function(z) {
      (z[1] %% 12 == bass_pc) &&
        !anyDuplicated(z)
    }) %>%
    unique
}
