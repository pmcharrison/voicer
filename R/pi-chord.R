#' @export
voice.vec_pi_chord <- function(x, opt = voice_opt()) {
  if (any(purrr::map_lgl(x, function(z) length(z) == 0L)))
    stop("empty chords not permitted")
  y <- all_voicings_vec_pi_chord(x = x, opt = opt)
  if (any(purrr::map_lgl(y, function(z) length(z) == 0L)))
    stop("no legal revoicings found")
  seqopt::seq_opt(y, cost_funs = opt$cost_funs, progress = opt$progress) %>%
    hrep::vec(type = "pi_chord")
}

all_voicings_vec_pi_chord <- function(x, opt) {
  purrr::map(x, function(y) all_voicings_pi_chord(y, opt))
}

#' All voicings
#'
#' Lists all the possible voicings for an object of class
#' \code{\link[hrep]{pi_chord}}.
#' @param x Object to voice.
#' @param opt Options list as created by \code{\link{voice_opt}}.
#' @return A list of possible voicings.
#' @export
all_voicings_pi_chord <- function(x, opt) {
  if (length(x) == 0L) stop("empty chords not permitted")
  x <- sort(x)
  bass_pc <- x[1] %% 12
  x <- if (opt$dbl_change) {
    pc_set <- sort(unique(x %% 12))
    all_voicings_pc_set(x = pc_set, opt)
  } else {
    pc_multiset <- sort(x %% 12)
    all_voicings_pc_multiset(x = pc_multiset, opt)
  }
  purrr::keep(x, function(z) (z[1] %% 12 == bass_pc))
}
