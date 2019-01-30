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
                      dbl_min = 5L,
                      dbl_max = 5L,
                      cost_funs = voicer_cost_funs(),
                      progress = interactive()) {
  stopifnot(min_octave <= max_octave)
  stopifnot(dbl_min <= dbl_max)
  as.list(environment())
}
