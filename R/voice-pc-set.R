voice_pc_set_seq <- function(x,
                             min_octave = -1L,
                             max_octave = 0L,
                             cost_funs = chord_cost_funs(),
                             progress = TRUE) {
  checkmate::qassert(x, "l")
  checkmate::qassert(min_octave, "X1")
  checkmate::qassert(max_octave, "X1")
  checkmate::qassert(cost_funs, "l")
  checkmate::qassert(progress, "B1")
  stopifnot(min_octave <= max_octave)

  y <- purrr::map(x, function(pc_set) all_pc_set_voicings(
    pc_set, min_octave = min_octave, max_octave = max_octave))

  if (any(purrr::map_lgl(y, function(z) length(z) == 1L)))
    stop("no legal revoicings found")

  seqopt::seq_opt(y, cost_funs = cost_funs, progress = progress)
}

#' Pitch-class set voicings
#'
#' Finds all possible voicings of a pitch-class set within a given octave range.
#' @param pc_set Numeric vector; the pitch-class set to voice.
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
#' @return A list of all possible voicings, expressed as numeric vectors
#' of MIDI note numbers.
#' @export
all_pc_set_voicings <- function(pc_set, min_octave, max_octave) {
  checkmate::qassert(pc_set, "N[0,12)")
  checkmate::qassert(min_octave, "X1")
  checkmate::qassert(max_octave, "X1")
  stopifnot(min_octave <= max_octave,
            !anyDuplicated(pc_set))

  octaves <- seq(from = min_octave, to = max_octave)
  spec <- gtools::permutations(n = length(octaves),
                               r = length(pc_set),
                               v = 60L + 12L * octaves,
                               repeats.allowed = TRUE)
  res <- sweep(x = spec, MARGIN = 2, STATS = pc_set, FUN = "+")
  purrr::map(seq_len(nrow(res)), function(i) sort(res[i, ]))
}
